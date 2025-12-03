module Analysis.Analysis (runAnalysis, getDataflowAnalysis) where
import Parse.AST
import qualified Analysis.Semantics as Semantics
import Data.Data
import Data.List (intercalate)
import Error (MicroCError(ESemanticAnalysis))
import Control.Exception
import Analysis.Error
import qualified Analysis.Typecheck.Typecheck as Typecheck
import qualified Analysis.Dataflow.Const as ConstAna
import qualified Analysis.Cfg.Builder as CFGBuilder

runAnalysis :: (Show a, Data a, Ord a) => Program a -> IO ()
runAnalysis prog = if null errors
        then pure ()
        else do
            let sx = intercalate "\n" (show <$> errors)
            throw $ ESemanticAnalysis sx
    where
        errors = (Semantic <$> Semantics.verify prog) <> (Type <$> Typecheck.verify prog)

getDataflowAnalysis :: Show a => Program a -> [(String, ConstAna.ResultMap)]
getDataflowAnalysis prog = constx
    where
        cfgx = zip (name <$> prog) (CFGBuilder.build <$> prog)
        constx = (\(n, cfg) -> (n, ConstAna.solve cfg)) <$> cfgx
