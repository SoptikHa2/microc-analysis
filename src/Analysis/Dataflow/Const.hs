{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Analysis.Dataflow.Const (solve, ConstResultMap, ConstResultLat, ConstLattice, latticeToInt) where
import Analysis.Dataflow.Analysis (ResultMap, ResultLat, runAnalysisOnVars)
import qualified Data.Map as M
import Parse.AST
import Lattice (Lattice(..))
import Data.Maybe
import Analysis.Cfg.Cfg

data ConstLattice
    = Top
    | Const Int
    | Bottom
    deriving (Eq)

latticeToInt :: ConstLattice -> Maybe Int
latticeToInt (Const i) = Just i
latticeToInt _ = Nothing

instance Show ConstLattice where
    show Top = "T"
    show Bottom = "⊥"
    show (Const i) = show i

instance Lattice ConstLattice where
    top = Top
    bottom = Bottom
    
    a <&> b | a == b = a
    Top <&> x = x
    x <&> Top = x
    Bottom <&> _ = Bottom
    _ <&> Bottom = Bottom
    Const _ <&> Const _ = Bottom

    a <|> b | a == b = a
    Top <|> _ = Top
    _ <|> Top = Top
    Bottom <|> x = x
    x <|> Bottom = x
    Const _ <|> Const _ = Top

type ConstResultMap = ResultMap ConstResultLat
type ConstResultLat = ResultLat ConstLattice

solve :: CFG a -> ConstResultMap
solve cfg = runAnalysisOnVars nextId prevId computeStmt cfg cfg.root.id

computeStmt :: Stmt a -> ConstResultLat -> ConstResultLat
-- The only one that matters is Assignment. Block should not appear (this is CFG!)
computeStmt (Block _ _) _ = error "Block statement remained in CFG. This is illegal"
computeStmt (AssignmentStmt _ (EIdentifier _ variable) rhs) lat = M.insert variable cRhs lat
    where
        cRhs = computeExpr rhs lat
computeStmt _ lat = lat

computeExpr :: Expr a -> ConstResultLat -> ConstLattice
computeExpr (BiOp _ op l r) lat = runBiOp op lhs rhs
    where
        lhs = computeExpr l lat
        rhs = computeExpr r lat
computeExpr (UnOp _ op e) lat = runUnOp op expr
    where
        expr = computeExpr e lat
computeExpr (Input _) _ = top
computeExpr (Null _) _ = bottom
computeExpr (FieldAccess _ _ _) _ = top
computeExpr (ArrayAccess _ _ _) _ = top
computeExpr (Call _ _ _) _ = top
computeExpr (Record _ _) _ = bottom
computeExpr (Array _ _) _ = bottom
computeExpr (Number _ i) _ = Const i
computeExpr (EIdentifier _ varId) lat = fromMaybe bottom (lat M.!? varId)

runUnOp :: UnOp -> ConstLattice -> ConstLattice
runUnOp Not (Const 0) = Const 1
runUnOp Not (Const _) = Const 0
runUnOp Not Top = Top
runUnOp _ _ = bottom

runBiOp :: BiOp -> ConstLattice -> ConstLattice -> ConstLattice
runBiOp _ Bottom _ = Bottom
runBiOp _ _ Bottom = Bottom
runBiOp _ Top _ = Top
runBiOp _ _ Top = Top

runBiOp Eq (Const l) (Const r) = if l == r then Const 1 else Const 0
runBiOp Gt (Const l) (Const r) = if l > r then Const 1 else Const 0
runBiOp Plus (Const l) (Const r) = Const (l + r)
runBiOp Minus (Const l) (Const r) = Const (l - r)
runBiOp Mul (Const l) (Const r) = Const (l * r)
runBiOp Div (Const _) (Const r) | r == 0 = Bottom
runBiOp Div (Const l) (Const r) = Const (l `div` r)
