{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Typecheck.ConstraintSolver where
import Analysis.Typecheck.Constraints (Constraints, Typeable)
import Analysis.Typecheck.Type (TypeError, Type(..))
import qualified Data.Map as M

-- mapping from unknown IDs into types
type Resolutions = M.Map Int Type

-- Resolve Unknown types in the constraints by unification
solve :: forall a . (Ord a) => Constraints a -> Either TypeError (M.Map (Typeable a) Type)
solve ctx = go typesPerTypable >>= resolveResult
    where
        -- First of all, for each typeable, we
        -- find the list of possible types they contain.
        -- Second, we repeat the following:
        --    we find the possible unknown types to union.
        --    For example, when variable X is of type
        --    both Unknown 1 and Int, then we know that
        --    averything with type Unknown 1 must be an int.
        --    So we write Unknown 1 -> Int mapping,
        --    and apply it everywhere.
        -- This continues, and only ends when no new merges
        -- are possible, or when we attempt to merge
        -- two incompatible types.
        --
        -- If all unknown variables have been replaced, we
        -- have the solution. Otherwise, something is
        -- untypable.
        typesPerTypable :: M.Map (Typeable a) [Type]
        typesPerTypable = M.fromListWith (++) ((\(k,v) -> (k,[v])) <$> ctx)

        go :: M.Map (Typeable a) [Type] -> Either TypeError (M.Map (Typeable a) [Type])
        go tpt | isFinal tpt = Right tpt
        go tpt = do
            -- merge
            -- if changed, repeat
            -- else, return
            undefined

        isFinal :: M.Map (Typeable a) [Type] -> Bool
        isFinal tpt = all 
            (\types -> all (\t -> case t of Unknown _ -> False; _ -> True) types)
            tpt
        

        resolveResult :: M.Map (Typeable a) [Type] -> Either TypeError (M.Map (Typeable a) Type)
        resolveResult = undefined

merge :: Type -> Type -> Either TypeError Type
merge t1 t2 | t1 == t2 = Right t1
merge t1@(Unknown _) (Unknown _) = Right t1
merge t1 (Unknown _) = Right t1
merge (Unknown _) t2 = Right t2
merge t1 t2 = Left $ "Cannot merge distinct types " <> show t1 <> " and " <> show t2


