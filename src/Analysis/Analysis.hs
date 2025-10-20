module Analysis.Analysis where
import Parse.AST
import qualified Analysis.Semantics as Semantics
import Control.Monad
import System.Exit (exitFailure)
import Data.Data

runAnalysis :: (Show a, Data a) => Program a -> IO ()
runAnalysis prog = if null errors
        then pure ()
        else do
            putStrLn "Analysis found some problems. Aborting run."
            forM_ errors print
            exitFailure
    where
        errors = Semantics.verify prog
