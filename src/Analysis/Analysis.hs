module Analysis.Analysis where
import Parse.AST
import qualified Analysis.Semantics as Semantics
import Data.Data
import Data.List
import Error (MicroCError(ESemanticAnalysis))
import Control.Exception

runAnalysis :: (Show a, Data a) => Program a -> IO ()
runAnalysis prog = if null errors
        then pure ()
        else do
            let sx = intercalate "\n" (show <$> errors)
            throw $ ESemanticAnalysis sx
    where
        errors = Semantics.verify prog
