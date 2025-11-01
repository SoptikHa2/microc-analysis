{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Typecheck.ConstraintSolver where
import Analysis.Typecheck.Constraints (Constraints, Typeable, typeableLoc)
import Analysis.Typecheck.Type (TypeError, Type(..))
import Control.Monad (zipWithM)
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

            -- For each typeable, merge all its types and extract substitutions
            substitutions <- findSubstitutions tpt

            -- If no substitutions found, we can't make progress
            if M.null substitutions
                then Left "Cannot resolve all unknown types"
                else do
                    -- Apply substitutions to all types
                    let tpt' = M.map (map (substitute substitutions)) tpt
                    go tpt'

        isFinal :: M.Map (Typeable a) [Type] -> Bool
        isFinal tpt = all
            (\types -> all (\t -> case t of Unknown _ -> False; _ -> True) types)
            tpt


        resolveResult :: M.Map (Typeable a) [Type] -> Either TypeError (M.Map (Typeable a) Type)
        resolveResult tpt = M.traverseWithKey finalizeType tpt
          where
            finalizeType :: (Typeable a) -> [Type] -> Either TypeError Type
            finalizeType _ [] = Left "No types for typeable"
            finalizeType _ [t] = Right t
            finalizeType typ types =
                -- After resolution, all types should be the same
                -- Merge them one final time to verify compatibility
                mergeAll types
              where
                mergeAll [] = Left "Empty type list"
                mergeAll [t] = Right t
                mergeAll (t1:t2:rest) = do
                    merged <- merge (typeableLoc typ) t1 t2
                    mergeAll (merged:rest)

merge :: (Show a) => a -> Type -> Type -> Either TypeError Type
merge _ t1 t2 | t1 == t2 = Right t1
merge _ t1@(Unknown _) (Unknown _) = Right t1
merge _ t1 (Unknown _) = Right t1
merge _ (Unknown _) t2 = Right t2
merge l (Ptr t1) (Ptr t2) = Ptr <$> merge l t1 t2
merge l (Fun args1 ret1) (Fun args2 ret2)
    | length args1 == length args2 = do
        mergedArgs <- zipWithM (merge l) args1 args2
        mergedRet <- merge l ret1 ret2
        return $ Fun mergedArgs mergedRet
    | otherwise = Left $ show l ++ ": Attempted to unify functions of arities " ++ show (length args1) ++ " and " ++ show (length args2)
merge l (Record fields1) (Record fields2) = do
    -- Records must have same field names
    let names1 = map fst fields1
        names2 = map fst fields2
    if names1 /= names2
        then Left $ show l ++ ": Cannot merge records with different fields (" ++ show names1 ++ " vs " ++ show names2 ++ ")"
        else do
            mergedFields <- zipWithM mergeField fields1 fields2
            return $ Record mergedFields
  where
    mergeField (n1, t1) (n2, t2)
        | n1 == n2 = (,) n1 <$> merge l t1 t2
        | otherwise = Left $ show l ++ ": Field name mismatch: " <> n1 <> " vs " <> n2
merge l t1 t2 = Left $ show l ++ ": Cannot merge distinct types " <> show t1 <> " and " <> show t2

-- Find substitutions by merging types for each typeable
findSubstitutions :: M.Map (Typeable a) [Type] -> Either TypeError Resolutions
findSubstitutions tpt = do
    substitutions <- M.traverseWithKey processTypeable tpt
    return $ M.unions substitutions
  where
    processTypeable :: (Typeable a) -> [Type] -> Either TypeError Resolutions
    processTypeable _ [] = Right M.empty
    processTypeable _ [_] = Right M.empty
    processTypeable typ types = do
        -- Merge all types for this typeable
        merged <- mergeAll (typeableLoc typ) types
        -- Extract substitutions: any Unknown in types that differs from merged
        return $ extractSubstitutions types merged

    mergeAll :: String -> [Type] -> Either TypeError Type
    mergeAll _ [] = Left "Empty type list"
    mergeAll _ [t] = Right t
    mergeAll l (t:ts) = do
        rest <- mergeAll l ts
        merge l t rest

    extractSubstitutions :: [Type] -> Type -> Resolutions
    extractSubstitutions types result =
        M.fromList [(uid, result) | Unknown uid <- types, Unknown uid /= result]

-- Apply substitutions recursively to a type
substitute :: Resolutions -> Type -> Type
substitute subst t@(Unknown uid) =
    case M.lookup uid subst of
        Just t' -> substitute subst t'  -- Follow chains
        Nothing -> t
substitute subst (Ptr t) = Ptr (substitute subst t)
substitute subst (Fun args ret) =
    Fun (map (substitute subst) args) (substitute subst ret)
substitute subst (Record fields) =
    Record [(name, substitute subst t) | (name, t) <- fields]
substitute _ t = t


