module Analysis.Analysis (runAnalysis, getConstAnalysis, getSignAnalysis) where
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
import Analysis.Cfg.Cfg (CFG)
import Analysis.Dataflow.Analysis (ResultMap)
import qualified Analysis.Dataflow.Sign as SignAna

runAnalysis :: (Show a, Data a, Ord a) => Program a -> IO ()
runAnalysis prog = if null errors
        then pure ()
        else do
            let sx = intercalate "\n" (show <$> errors)
            throw $ ESemanticAnalysis sx
    where
        errors = (Semantic <$> Semantics.verify prog) <> (Type <$> Typecheck.verify prog)

getDataflowAnalysis :: (CFG a -> ResultMap l) -> Program a -> [(String, CFG a, ResultMap l)]
getDataflowAnalysis solve prog = constx
    where
        cfgx = zip (name <$> prog) (CFGBuilder.build <$> prog)
        constx = (\(n, cfg) -> (n, cfg, solve cfg)) <$> cfgx

getConstAnalysis :: Program a -> [(String, CFG a, ResultMap ConstAna.ConstLattice)]
getConstAnalysis = getDataflowAnalysis ConstAna.solve

getSignAnalysis :: Program a -> [(String, CFG a, ResultMap SignAna.SignLattice)]
getSignAnalysis = getDataflowAnalysis SignAna.solve
