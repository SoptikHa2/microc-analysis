module Analysis.Dataflow.Sign where
import Lattice (Lattice(..))
import Analysis.Dataflow.Analysis
import Parse.AST
import Analysis.Cfg.Cfg
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data SignLattice
    = Top
    | Pos
    | Zer
    | Neg
    | Bottom
    deriving (Eq)

instance Show SignLattice where
    show Top = "T"
    show Bottom = "⊥"
    show Pos = "+"
    show Zer = "0"
    show Neg = "-"

instance Lattice SignLattice where
    top = Top
    bottom = Bottom

    a <&> b | a == b = a
    Top <&> x = x
    x <&> Top = x
    Bottom <&> _ = Bottom
    _ <&> Bottom = Bottom
    _ <&> _ = Bottom  -- two different

    a <|> b | a == b = a
    Top <|> _ = Top
    _ <|> Top = Top
    Bottom <|> x = x
    x <|> Bottom = x
    _ <|> _ = Top

type SignResultMap = ResultMap SignResultLat
type SignResultLat = ResultLat SignLattice

solve :: CFG a -> SignResultMap
solve cfg = runAnalysisOnVars nextId prevId computeStmt cfg cfg.root.id

computeStmt :: Stmt a -> SignResultLat -> SignResultLat
-- The only one that matters is Assignment. Block should not appear (this is CFG!)
computeStmt (Block _ _) _ = error "Block statement remained in CFG. This is illegal"
computeStmt (AssignmentStmt _ (EIdentifier _ variable) rhs) lat = M.insert variable cRhs lat
    where
        cRhs = computeExpr rhs lat
computeStmt _ lat = lat

computeExpr :: Expr a -> SignResultLat -> SignLattice
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
computeExpr (Number _ 0) _ = Zer
computeExpr (Number _ i) _ | i > 0 = Pos
computeExpr (Number _ i) _ | i < 0 = Neg
computeExpr (EIdentifier _ varId) lat = fromMaybe bottom (lat M.!? varId)

runUnOp :: UnOp -> SignLattice -> SignLattice
runUnOp Not Zer = Pos   -- !0 = 1
runUnOp Not Pos = Zer   -- !positive = 0
runUnOp Not Neg = Zer   -- !negative = 0
runUnOp Not Top = Top   -- could be 0 or 1
runUnOp _ _ = bottom

runBiOp :: BiOp -> SignLattice -> SignLattice -> SignLattice
runBiOp _ Bottom _ = Bottom
runBiOp _ _ Bottom = Bottom
runBiOp _ Top _ = Top
runBiOp _ _ Top = Top

runBiOp Eq Zer Zer = Pos
runBiOp Eq a b | a /= b = Zer
runBiOp Eq _ _ = Top

runBiOp Gt Pos Zer = Pos
runBiOp Gt Pos Neg = Pos
runBiOp Gt Zer Pos = Zer
runBiOp Gt Zer Zer = Zer
runBiOp Gt Zer Neg = Pos
runBiOp Gt Neg Zer = Zer
runBiOp Gt Neg Pos = Zer
runBiOp Gt _ _ = Top

runBiOp Plus Zer x = x
runBiOp Plus x Zer = x
runBiOp Plus Pos Neg = Top
runBiOp Plus Neg Pos = Top
runBiOp Plus Pos Pos = Pos
runBiOp Plus Neg Neg = Neg

runBiOp Minus Zer Pos = Neg
runBiOp Minus Zer Neg = Pos
runBiOp Minus x Zer = x
runBiOp Minus Neg Pos = Neg
runBiOp Minus Pos Neg = Pos
runBiOp Minus _ _ = Top

runBiOp Mul Zer _ = Zer
runBiOp Mul _ Zer = Zer
runBiOp Mul a b | a == b = Pos
runBiOp Mul _ _ = Neg

runBiOp Div _ Zer = Bottom
runBiOp Div a b = runBiOp Mul a b
