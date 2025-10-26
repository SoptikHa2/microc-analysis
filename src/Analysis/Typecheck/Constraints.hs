module Analysis.Typecheck.Constraints (Typeable(..), Constraints) where
import Parse.AST
import Analysis.Typecheck.Type

data Typeable a
    = CExpr (Expr a)
    | CFun (FunDecl a)
    | CId Identifier
    deriving (Show, Eq, Ord)

type Constraints a = [(Typeable a, Type)]
