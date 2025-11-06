{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Typecheck.Typecheck (verify, getTyping) where
import Data.Data (Data)
import Parse.AST
import Analysis.Typecheck.Type
import Control.Monad.Identity (runIdentity)
import qualified Data.Map as M
import Analysis.Typecheck.Constraints
import Analysis.Typecheck.ConstraintSolver (solve)
import Data.List (intercalate)
import Data.Generics.Uniplate.Data (universeBi)
import Analysis.Typecheck.ConstraintGenerator
import Control.Monad.State


getAllFieldsNames :: forall a . (Data a) => FunDecl a -> [Identifier]
getAllFieldsNames fun = fieldNames
    where
        fieldCtors = [fields | Parse.AST.Record (_ :: a) (Fields fields) <- universeBi fun]
        fieldNames = fst <$> concat fieldCtors

verify :: (Show a, Data a, Ord a) => [FunDecl a] -> [TypeError]
verify funcs = do
    -- Generate constraints per function
    let fn = concat $ getAllFieldsNames <$> funcs
    let (cx, _state) = runIdentity (runStateT (traverse genConstraintsFun funcs) (emptyState fn))

    case solve (concat cx) of
        Left te -> [te]
        Right _ -> []

getTyping :: (Show a, Data a, Ord a) => [FunDecl a] -> Either TypeError (M.Map (Typeable a) Type)
getTyping funcs = do
    -- Generate constraints per function
    let fn = concat $ getAllFieldsNames <$> funcs
    let (cx, _state) = runIdentity (runStateT (traverse genConstraintsFun funcs) (emptyState fn))
    solve (concat cx)
