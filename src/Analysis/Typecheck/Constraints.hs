{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
module Analysis.Typecheck.Constraints (Typeable(..), Constraints, typeableLoc, prettyPrintMTT, prettyPrintCX, printTyping, printAllTyping) where
import Parse.AST
import Analysis.Typecheck.Type
import Data.List (intercalate)
import qualified Data.Map as M

data Typeable a
    = CExpr (Expr a)
    | CFun (FunDecl a)
    | CId String Identifier
    deriving (Show, Eq, Ord)

typeableLoc :: Show a => Typeable a -> String
typeableLoc (CExpr e) = show $ exprData e
typeableLoc (CFun f) = show f.d
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
printTyping = printTyping' False
printAllTyping :: forall a . (Show a) => (M.Map (Typeable a) Type) -> String
printAllTyping = printTyping' True

-- bool: include expr types?
printTyping' :: forall a . (Show a) => Bool -> (M.Map (Typeable a) Type) -> String
printTyping' includeExpr m = intercalate "\n" (filter (/= "") (M.elems $ M.mapWithKey go m))
    where
        go :: Show a => Typeable a -> Type -> String
        go ty@(CExpr _) t | includeExpr = "[Expression at " ++ typeableLoc ty ++ "] = " ++ show t
        go (CExpr _) _ | not includeExpr = ""
        go ty@(CFun f) t = "[" ++ f.name ++ "() :: " ++ typeableLoc ty ++ "] = " ++ show t
        go (CId l i) t = "[" ++ i ++ " :: " ++ l ++ "] = " ++ show t
