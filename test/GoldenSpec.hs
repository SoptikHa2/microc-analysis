module GoldenSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden (Golden(..))
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Data.List (sort, intercalate)
import Text.Parsec (SourcePos)
import qualified Data.Map as M

import Parse.AST (Program)
import Workflow (getSource, SourceData(..), getAna, AnalysisData(..))
import Analysis.Typecheck.Constraints (printTyping)
import Analysis.Analysis (getConstAnalysis, getSignAnalysis, getVeryBusyAnalysis, getReachingDefsAnalysis, getLivenessAnalysis)
import Analysis.Dataflow.Analysis (ResultMap)
import qualified Analysis.Dataflow.Utils as DFUtils
import qualified Analysis.Cfg.Cfg as CFG
import qualified Compile.Compile as C
import Utils ((<$$>))

examplesDir :: FilePath
examplesDir = "examples"

goldenDir :: FilePath
goldenDir = "test/golden"

-- | Custom golden that uses our golden directory
golden :: String -> String -> Golden String
golden name actualOutput = Golden
    { output = actualOutput
    , encodePretty = id
    , writeToFile = writeFile
    , readFromFile = readFile
    , goldenFile = goldenDir </> name <> ".golden"
    , actualFile = Just (goldenDir </> name <> ".actual")
    , failFirstTime = False
    }

-- | Discover all .mc files in the examples directory
discoverExamples :: IO [FilePath]
discoverExamples = do
    files <- listDirectory examplesDir
    let mcFiles = filter (\f -> takeExtension f == ".mc") files
    pure $ sort mcFiles

-- | Load and parse a source file
loadSource :: FilePath -> IO SourceData
loadSource filepath = do
    code <- readFile filepath
    case getSource code filepath of
        Left err -> fail $ "Parse/type error: " ++ show err
        Right source -> pure source

-- | Format analysis output (same as Main.hs)
formatAnalysis :: (d -> String) -> [(String, CFG.CFG a, ResultMap d)] -> String
formatAnalysis show' results = intercalate "\n" $ do
    (funName, cfg, resultMap) <- results
    [ "Function: " ++ funName
      , DFUtils.prettyPrintAnalysis show' cfg resultMap
      , ""
      ]

-- | Get type checking output
getTypeOutput :: SourceData -> String
getTypeOutput source = printTyping source.typing

-- | Get const analysis output
getConstOutput :: Program SourcePos -> String
getConstOutput prog = formatAnalysis DFUtils.formatResultLat (getConstAnalysis prog)

-- | Get sign analysis output
getSignOutput :: Program SourcePos -> String
getSignOutput prog = formatAnalysis DFUtils.formatResultLat (getSignAnalysis prog)

-- | Get very busy analysis output
getVeryBusyOutput :: Program SourcePos -> String
getVeryBusyOutput prog = formatAnalysis show (getVeryBusyAnalysis prog)

-- | Get reaching definitions analysis output
getReachOutput :: Program SourcePos -> String
getReachOutput prog = formatAnalysis show (getReachingDefsAnalysis prog)

-- | Get liveness analysis output
getLiveOutput :: Program SourcePos -> String
getLiveOutput prog = formatAnalysis show (getLivenessAnalysis prog)

-- | Get CFG output
getCfgOutput :: SourceData -> String
getCfgOutput source =
    let ana = getAna source.ast
        nameAndCfg = M.toList $ cfg <$> ana
        dots = uncurry CFG.cfgshow <$> nameAndCfg
    in intercalate "\n\n" dots

-- | Get compilation output
getCompileOutput :: SourceData -> String
getCompileOutput source =
    case C.compile source.ast of
        Left err -> error $ "Compile error: " ++ err
        Right asm -> asm

-- | Create specs for a single example file
specForExample :: FilePath -> Spec
specForExample filename = describe filename $ do
    let baseName = takeBaseName filename
        srcPath = examplesDir </> filename

    -- Load source once for this file
    source <- runIO $ loadSource srcPath
    let prog = fst <$$> source.ast

    it "type checking" $
        golden (baseName ++ ".type") (getTypeOutput source)

    it "const analysis" $
        golden (baseName ++ ".const") (getConstOutput prog)

    it "sign analysis" $
        golden (baseName ++ ".sign") (getSignOutput prog)

    it "very busy analysis" $
        golden (baseName ++ ".vbusy") (getVeryBusyOutput prog)

    it "reaching definitions analysis" $
        golden (baseName ++ ".reach") (getReachOutput prog)

    it "liveness analysis" $
        golden (baseName ++ ".live") (getLiveOutput prog)

    it "CFG generation" $
        golden (baseName ++ ".cfg") (getCfgOutput source)

    it "compilation" $
        golden (baseName ++ ".asm") (getCompileOutput source)

spec :: Spec
spec = do
    runIO $ createDirectoryIfMissing True goldenDir
    examples <- runIO discoverExamples
    if null examples
        then it "no examples found" $
            pendingWith "Add .mc files to the examples/ directory"
        else mapM_ specForExample examples
