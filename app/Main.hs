module Main (main) where

import Options.Applicative hiding (empty)
import Control.Monad (forM_)
import System.Exit (exitFailure, exitWith, ExitCode (ExitFailure), exitSuccess)
import Text.Parsec (parse, SourcePos)

import Parse.DeclParser (program)
import Parse.AST hiding (name, args)
import Interpreter.Data (Value(..))
import Analysis.Typecheck.Constraints (printTyping)
import Control.Exception
import Error
import Analysis.Cfg.Cfg as CFG
import Data.List (intercalate)
import Analysis.Analysis (getConstAnalysis, getSignAnalysis, getVeryBusyAnalysis, getReachingDefsAnalysis)
import qualified Analysis.Dataflow.Utils as DFUtils
import Analysis.Dataflow.Analysis (ResultMap)
import qualified Compile.Compile as C
import Workflow (getSource, ast, SourceData (typing), getAna, cfg)
import Interpreter.InterpretRun (interpretIO)
import Utils ((<$$>))
import qualified Data.Map as M

-- CLI data types
data Command
  = Run FilePath [Int]
  | Type FilePath
  | Cfg FilePath
  | ConstAna FilePath
  | SignAna FilePath
  | VeryBusyAna FilePath
  | ReachAna FilePath
  | Compile FilePath (Maybe FilePath)
  | Asm FilePath

-- Parser for command line arguments
commandParser :: Parser Command
commandParser = hsubparser
    (  command "run" (info runParser (progDesc "Run a MicroC program"))
    <> command "type" (info typeParser (progDesc "Type check a MicroC program"))
    <> command "cfg" (info cfgParser (progDesc "Generate CFG of a program"))
    <> command "const" (info constParser (progDesc "Run const propagation analysis of the program"))
    <> command "sign" (info signParser (progDesc "Run sign propagation analysis of the program"))
    <> command "vbusy" (info veryBusyParser (progDesc "Run very busy analysis of the program"))
    <> command "reach" (info reachParser (progDesc "Run reaching definitions analysis of the program"))
    <> command "compile" (info compileParser (progDesc "Compile program into .t86"))
    <> command "asm" (info asmParser (progDesc "Compile program into .t86 and output the assembly"))
    )
  where
    programArg = argument str (metavar "PROGRAM" <> help "Path to the MicroC source file")

    runParser = Run
      <$> programArg
      <*> many (argument auto (metavar "ARGS..." <> help "Integer arguments to main function"))
    typeParser = Type <$> programArg
    cfgParser = Cfg <$> programArg
    constParser = ConstAna <$> programArg
    signParser = SignAna <$> programArg
    veryBusyParser = VeryBusyAna <$> programArg
    reachParser = ReachAna <$> programArg
    compileParser = Compile <$> programArg <*> optional (argument str (metavar "TARGET" <> help "Target output file (- for stdout; default: <input>.out)"))
    asmParser = Asm <$> programArg

-- Main entry point
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Run filepath args -> runProgram filepath args
    Type filepath -> typeCheckProgram filepath
    Cfg filepath -> generateCfg filepath
    ConstAna filepath -> runConsts filepath
    SignAna filepath -> runSign filepath
    VeryBusyAna filepath -> runVeryBusy filepath
    ReachAna filepath -> runReach filepath
    Compile filepath targetFile -> compile filepath outFile
      where
        outFile = case targetFile of
          Just "-" -> Nothing
          Just a -> Just a
          Nothing -> Just $ filepath <> ".out"
    Asm filepath -> compile filepath Nothing
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "MicroC interpreter - run MicroC programs"
     <> header "microc - A simple interpreter for the MicroC language" )

-- TODO: the rest of this file is a disaster and should be rewritten

_resIntoIO :: Show e => Either e a -> IO a
_resIntoIO (Right result) = pure result
_resIntoIO (Left e) = do
  print e
  exitWith $ ExitFailure 1

runProgram :: FilePath -> [Int] -> IO ()
runProgram filepath args = go `catch` \e -> do
    print (e :: MicroCError)
    exitWith $ ExitFailure 1
  where
    go = do
      code <- readFile filepath
      source <- _resIntoIO $ getSource code filepath
      resultVal <- interpretIO (fst <$$> source.ast) args
      -- Exit with the status of main
      case resultVal of
        (VNumber exitCode) | (exitCode `mod` 256) == 0 -> exitSuccess
        (VNumber exitCode) -> exitWith (ExitFailure (exitCode `mod` 256))
        _ -> do
          putStrLn $ "Unknown return value from main: " <> show resultVal
          exitWith (ExitFailure 1)

-- Type check a MicroC program
typeCheckProgram :: FilePath -> IO ()
typeCheckProgram filepath = do
  code <- readFile filepath
  source <- _resIntoIO $ getSource code filepath
  putStrLn $ printTyping source.typing
  exitSuccess

generateCfg :: FilePath -> IO ()
generateCfg filepath = do
  code <- readFile filepath
  source <- _resIntoIO $ getSource code filepath
  let ana = getAna source.ast
  let nameAndCfg = M.toList $ cfg <$> ana
  let dots = uncurry CFG.cfgshow <$> nameAndCfg
  putStrLn (intercalate "\n\n" dots)

compile :: FilePath -> Maybe FilePath -> IO ()
compile filepath target = do
  code <- readFile filepath
  source <- _resIntoIO $ getSource code filepath
  asm <- _resIntoIO $ C.compile source.ast
  case target of
    Just target -> writeFile target asm
    Nothing -> putStrLn asm

runAna :: (d -> String) -> (Program SourcePos -> [(String, CFG a, ResultMap d)]) -> String -> IO ()
runAna show' op filepath = do
  -- TODO: refactor to use analysis workflow
  code <- readFile filepath
  source <- _resIntoIO $ getSource code filepath
  let results = op (fst <$$> source.ast)
  forM_ results $ \(funName, cfg, resultMap) -> do
    putStrLn $ "Function: " ++ funName
    putStrLn $ DFUtils.prettyPrintAnalysis show' cfg resultMap
    putStrLn ""

runConsts = runAna DFUtils.formatResultLat getConstAnalysis

runSign = runAna DFUtils.formatResultLat getSignAnalysis

runVeryBusy = runAna show getVeryBusyAnalysis

runReach = runAna show getReachingDefsAnalysis

