module Main (main) where

import Options.Applicative hiding (empty)
import Control.Monad.State (StateT, evalStateT, execStateT, mapStateT)
import Control.Monad (forM_)
import Control.Monad.Identity (Identity, runIdentity)
import System.Exit (exitFailure, exitWith, ExitCode (ExitFailure), exitSuccess)
import Text.Parsec (parse, SourcePos)

import Parse.DeclParser (program)
import Parse.AST hiding (name, args)
import qualified Interpreter.State as IS
import Interpreter.Interpret (evalFun)
import Interpreter.Data (Value(..))
import Analysis.Analysis (runAnalysis)
import Control.Exception
import Error

-- CLI data types
data Command = Run FilePath [Int]

-- Parser for command line arguments
commandParser :: Parser Command
commandParser = hsubparser
  ( command "run" (info runParser (progDesc "Run a MicroC program"))
  )
  where
    runParser = Run
      <$> argument str (metavar "PROGRAM" <> help "Path to the MicroC source file")
      <*> many (argument auto (metavar "ARGS..." <> help "Integer arguments to main function"))

-- Main entry point
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Run filepath args -> runProgram filepath args
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "MicroC interpreter - run MicroC programs"
     <> header "microc - A simple interpreter for the MicroC language" )

-- Run a MicroC program
runProgram :: FilePath -> [Int] -> IO ()
runProgram filepath args = go `catch` \e -> do
    print (e :: MicroCError)
    exitWith $ ExitFailure 1
  where
    go = do
      -- Read the source file
      source <- readFile filepath

      -- Parse the program
      case parse program filepath source of
        Left err -> do
          putStrLn $ "Parse error: " ++ show err
          exitFailure
        Right prog -> do
          -- First of all, analyse it
          _ <- runAnalysis prog

          -- Find the main function
          case findFunction "main" prog of
            Nothing -> do
              putStrLn "Error: No 'main' function found in program"
              exitFailure
            Just mainFun -> do
              -- Check argument count matches
              let FunDecl loc _ funArgs _ = mainFun
              let expectedArgs = length funArgs
              let providedArgs = length args
              if expectedArgs /= providedArgs
                then do
                  putStrLn $ "Error: main function expects " ++ show expectedArgs
                          ++ " arguments, but " ++ show providedArgs ++ " were provided"
                  exitFailure
                else do
                  -- Initialize state with all functions in global scope
                  initialState <- initializeState prog

                  -- Run the main function with provided arguments
                  result <- evalStateT (evalFun mainFun loc (map VNumber args)) initialState

                  -- Exit with the status of main
                  case result of
                    (VNumber exitCode) | (exitCode `mod` 256) == 0 -> exitSuccess
                    (VNumber exitCode) -> exitWith (ExitFailure (exitCode `mod` 256))
                    _ -> do
                      putStrLn $ "Unknown return value from main: " <> show result
                      exitWith (ExitFailure 1)


-- Find a function by name in the program
findFunction :: Identifier -> Program a -> Maybe (FunDecl a)
findFunction name = foldr go Nothing
  where
    go f@(FunDecl _ fname _ _) acc
      | fname == name = Just f
      | otherwise = acc

-- Initialize the interpreter state with all functions in global scope
initializeState :: Program SourcePos -> IO IS.State
initializeState prog = do
  -- Start with empty state and add all functions to global scope
  let addFunctionsAction :: StateT IS.State IO ()
      addFunctionsAction = forM_ prog $ \f@(FunDecl _ fname _ _) ->
        liftIdentityToIO $ IS.putsGlobal fname (Function f)
  execStateT addFunctionsAction IS.empty
  where
    liftIdentityToIO :: StateT IS.State Identity a -> StateT IS.State IO a
    liftIdentityToIO = mapStateT (pure . runIdentity)
