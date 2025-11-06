{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Analysis.Typecheck.Constraints (Typeable(..), Constraints, typeableLoc, prettyPrintMTT, prettyPrintCX, printTyping) where
import Parse.AST
import Analysis.Typecheck.Type
import Data.List (intercalate)
import qualified Data.Map as M

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

printTyping :: forall a . (Show a) => (M.Map (Typeable a) Type) -> String
printTyping m = intercalate "\n" (filter (/= "") (M.elems $ M.mapWithKey go m))
    where
        go :: Show a => (Typeable a) -> Type -> String
        go (CExpr _ _) _ = ""
        go (CFun l f) t = "[" ++ f.name ++ "() :: " ++ l ++ "] = " ++ show t
        go (CId l i) t = "[" ++ i ++ " :: " ++ l ++ "] = " ++ show t
