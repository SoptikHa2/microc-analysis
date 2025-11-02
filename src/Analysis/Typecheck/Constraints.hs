{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Analysis.Typecheck.Constraints (Typeable(..), Constraints, typeableLoc, prettyPrintMTT, prettyPrintCX) where
import Parse.AST
import Analysis.Typecheck.Type
import Data.List (intercalate)

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

prettyPrintMTT :: Show a => [(Typeable a, [Type])] -> String
prettyPrintMTT m = intercalate "\n" $ pp <$> m
    where
        pp :: Show a => (Typeable a, [Type]) -> String
        pp (tp, t) = show tp ++ " :: " ++ show t

prettyPrintCX :: Show a => Constraints a -> String
prettyPrintCX cx = prettyPrintMTT ((\(ty, tp) -> (ty, [tp])) <$> cx)
