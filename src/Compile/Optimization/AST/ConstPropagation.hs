module Compile.Optimization.AST.ConstPropagation (optimize) where
import Workflow (AnalysisData (stmtToCfg, consts))
import Parse.AST (FunDecl, Stmt (AssignmentStmt), Expr (..), stmtData, exprData)
import Text.Parsec (SourcePos)
import Analysis.Typecheck.Type (Type)
import Data.Generics.Uniplate.Data (transformBi)
import Analysis.Dataflow.Const (ConstResultLat, latticeToInt)
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)

optimize :: AnalysisData -> FunDecl (SourcePos, Type) -> FunDecl (SourcePos, Type)
optimize analysis = transformBi (optimizeStmt analysis)

optimizeStmt :: AnalysisData -> Stmt (SourcePos, Type) -> Stmt (SourcePos, Type)
optimizeStmt adata stmt@(AssignmentStmt stmtData lhs@(EIdentifier _ lhsName) rhs)
    | isJust ival
     =
        AssignmentStmt stmtData lhs (Number (exprData rhs) (fromJust ival))
    where
        ana = getAna adata stmt
        ival = (ana M.!? lhsName) >>= latticeToInt
optimizeStmt _ stmt = stmt

getAna :: AnalysisData -> Stmt (SourcePos, Type) -> ConstResultLat
getAna adata stmt = adata.consts M.! cfgid
    where
        cfgid = adata.stmtToCfg M.! stmtData stmt
