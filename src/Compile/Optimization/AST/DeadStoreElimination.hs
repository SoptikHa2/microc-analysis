{-# LANGUAGE ScopedTypeVariables #-}
module Compile.Optimization.AST.DeadStoreElimination (optimize) where
import Workflow (AnalysisData (..))
import Parse.AST (FunDecl, Stmt (..), Expr (..), Identifier)
import Text.Parsec (SourcePos)
import Analysis.Typecheck.Type (Type)
import Data.Generics.Uniplate.Data (transformBi, universeBi)
import Analysis.Dataflow.Liveness (LivenessLattice(..))
import Analysis.Cfg.Cfg (nextId, CFG (..))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Data (Data)

optimize :: AnalysisData -> FunDecl (SourcePos, Type) -> FunDecl (SourcePos, Type)
optimize adata = transformBi (filter (not . isDeadStore adata))

isDeadStore :: AnalysisData -> Stmt (SourcePos, Type) -> Bool
isDeadStore adata (AssignmentStmt d (EIdentifier _ var) rhs)
    | isPure rhs = not $ isLiveAfter adata d var
isDeadStore _ _ = False

isLiveAfter :: AnalysisData -> (SourcePos, Type) -> Identifier -> Bool
isLiveAfter adata d var = any varIsLive succLiveness
    where
        cfgId = adata.stmtToCfg M.! d
        cfgNode = adata.cfg.idmap M.! cfgId
        succLiveness = (adata.liveness M.!) <$> nextId cfgNode
        varIsLive Top = True
        varIsLive (Live s) = var `S.member` s

isPure :: forall a. Data a => Expr a -> Bool
isPure e = null [() | Input (_ :: a) <- universeBi e]
        && null [() | Call (_ :: a) _ _ <- universeBi e]
