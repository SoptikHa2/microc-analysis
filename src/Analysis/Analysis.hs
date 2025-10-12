module Analysis.Analysis where
import Parse.AST
import qualified Analysis.Semantics as Semantics
import Control.Monad
import System.Exit (exitFailure)

runAnalysis :: Program -> IO ()
runAnalysis prog = if null errors
        then pure ()
        else do
            putStrLn "Analysis found some problems. Aborting run."
            forM_ errors print
            exitFailure
    where
        errors = Semantics.verify prog
