module Analysis.Analysis (runAnalysis) where
import Parse.AST
import qualified Analysis.Semantics as Semantics
import Data.Data
import Data.List (intercalate)
import Error (MicroCError(ESemanticAnalysis))
import Control.Exception
import qualified Analysis.Typecheck.Typecheck as Typecheck
import Analysis.Error

runAnalysis :: (Show a, Data a) => Program a -> IO ()
runAnalysis prog = if null errors
        then pure ()
        else do
            let sx = intercalate "\n" (show <$> errors)
            throw $ ESemanticAnalysis sx
    where
        errors = (Semantic <$> Semantics.verify prog) <> (Type <$> Typecheck.verify prog)
