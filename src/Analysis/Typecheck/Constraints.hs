{-# LANGUAGE RankNTypes #-}
module Analysis.Typecheck.Constraints (Typeable(..), Constraints, typeableLoc) where
import Parse.AST
import Analysis.Typecheck.Type

data Typeable a
    = CExpr String (Expr a)
    | CFun String (FunDecl a)
    | CId String Identifier
    deriving (Show, Eq, Ord)

typeableLoc :: Typeable a -> String
typeableLoc (CExpr l _) = l
typeableLoc (CFun l _) = l
typeableLoc (CId l _) = l

type Constraints a = [(Typeable a, Type)]
