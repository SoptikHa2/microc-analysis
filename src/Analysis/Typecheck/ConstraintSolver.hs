{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Typecheck.ConstraintSolver where
import Analysis.Typecheck.Constraints (Constraints, Typeable, typeableLoc, prettyPrintMTT)
import Analysis.Typecheck.Type (TypeError, Type(..))
import Control.Monad (zipWithM)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Data.Generics.Uniplate.Data (universeBi, transform)
import Data.List (sortBy)

-- mapping from unknown IDs into types
type Resolutions = M.Map Int Type

-- Resolve Unknown types in the constraints by unification
solve :: forall a . (Ord a, Show a) => Constraints a -> Either TypeError (M.Map (Typeable a) Type)
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
                -- Give up, some typing may not be completed, there may be unknown types left. Oh well.
                then Right tpt
                else do
                    -- Apply substitutions to all types and clean up
                    let tpt' = M.map (map (cleanupType . substitute substitutions)) tpt
                    go (trace ("\n---------\n" <> prettyPrintMTT (M.toList tpt')) tpt')

        isFinal :: M.Map (Typeable a) [Type] -> Bool
        isFinal tpt = not (any (any isUnknown) tpt)
            where
                isUnknown :: Type -> Bool
                isUnknown (Unknown _) = True
                isUnknown (Ptr t) = isUnknown t
                isUnknown (Fun tx t) = any isUnknown (t:tx)
                isUnknown (Record fx) = any (isUnknown <$> snd) fx
                isUnknown _ = False


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

merge :: String -> Type -> Type -> Either TypeError Type
merge l Bottom t2 = Left $ l ++ ": Record does not contain given field, attempted to use as " ++ show t2
merge l t1 Bottom = Left $ l ++ ": Record does not contain given field, attempted to use as " ++ show t1
merge _ t1 t2 | t1 == t2 = Right t1
merge _ t1@(Unknown _) (Unknown _) = Right t1
merge _ t1 (Unknown _) = Right t1
merge _ (Unknown _) t2 = Right t2
merge _ (BoundTypeVar i1) (BoundTypeVar i2) | i1 == i2 = Right (BoundTypeVar i1)
-- Recursive types
merge l (TypeVarBinding i1 body1) (TypeVarBinding i2 body2) = do
        -- TODO: this is wrong
        let normalizedBody2 = replaceBoundVar i2 i1 body2
        mergedBodies <- merge l body1 normalizedBody2
        Right (TypeVarBinding i1 mergedBodies)
-- send bound type vars to left
merge l t1 t2@(TypeVarBinding _ _) = merge l t2 t1
merge l (TypeVarBinding i1 body1) t2 =
    -- First try to merge the body directly
    -- TODO: This is weird and maybe wrong
    case merge l body1 t2 of
        Right mergedBody -> Right (TypeVarBinding i1 mergedBody)
        Left _ ->
            -- If that fails, try unfolding once
            case merge l (unfoldOnce i1 body1) t2 of
                Right unfolded -> Right (TypeVarBinding i1 unfolded)
                Left err2 -> Left err2
-- Ptr
merge l (Ptr t1) (Ptr t2) = Ptr <$> merge l t1 t2
-- Fun
merge l (Fun args1 ret1) (Fun args2 ret2)
    | length args1 == length args2 = do
        mergedArgs <- zipWithM (merge l) args1 args2
        mergedRet <- merge l ret1 ret2
        return $ Fun mergedArgs mergedRet
    | otherwise = Left $ l ++ ": Function being called has arity " ++ show (length args1) ++ ", but should have " ++ show (length args2)
-- Record
merge l (Record fields1) (Record fields2) = do
    let sfields1 = sortBy (\a b -> compare (fst a) (fst b)) fields1
    let sfields2 = sortBy (\a b -> compare (fst a) (fst b)) fields2
    mergedFields <- zipWithM mergeField sfields1 sfields2
    return $ Record mergedFields
  where
    mergeField (n1, t1) (n2, t2)
        | n1 == n2 = (,) n1 <$> merge l t1 t2
        | otherwise = Left $ l ++ ": Field name mismatch: " <> n1 <> " vs " <> n2
merge l t1 t2 = Left $ l ++ ": The type is " <> show t1 <> ", but should be " <> show t2

-- Unfold a recursive type
unfoldOnce :: Int -> Type -> Type
unfoldOnce i body = transform f body
  where
    f (BoundTypeVar j) | i == j = TypeVarBinding i body
    f x = x

-- Replace all occurrences of BoundTypeVar with one ID to another ID
replaceBoundVar :: Int -> Int -> Type -> Type
replaceBoundVar from to = transform f
  where
    f (BoundTypeVar j) | j == from = BoundTypeVar to
    f x = x

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
    extractSubstitutions types result = do
        let extras = case result of
                (Ptr rt) ->
                    let
                        ptrTypes = [t | Ptr t <- types]
                    in
                        extractSubstitutions ptrTypes rt
                (Fun resultArgs resultRet) ->
                    let
                        funTypes = [(args, ret) | Fun args ret <- types]
                        -- Extract substitutions from all argument positions
                        argSubsts = mconcat [extractSubstitutions (map (!! i) (fst <$> funTypes)) (resultArgs !! i)
                                           | i <- [0..length resultArgs - 1]]
                        -- Extract substitutions from return type
                        retSubsts = extractSubstitutions (snd <$> funTypes) resultRet
                    in
                        argSubsts <> retSubsts
                (Record fx) ->
                    let
                        recTypes = [rfx | Record rfx <- types]
                        allNames = fst <$> fx

                        perNameFx n = fromJust $ lookup n fx
                        perNameRt n = fromJust <$> lookup n <$> recTypes

                        substs n = extractSubstitutions (perNameRt n) (perNameFx n)
                        subst = substs <$> allNames
                    in
                        mconcat subst
                _ -> M.empty

        extras <> M.fromList [(uid, result) | Unknown uid <- types, Unknown uid /= result]

-- Remove useless type var bindings
cleanupType :: Type -> Type
cleanupType = transform f
    where
        hasBoundTypeVar tv typ = tv `elem` typeVars
            where
                typeVars = [i | BoundTypeVar i <- universeBi typ]

        f tv@(TypeVarBinding i t) | hasBoundTypeVar i t = tv
        f    (TypeVarBinding _ t)                       = t
        f x = x

-- Apply substitutions recursively to a type
-- If the replacement contains any unknown, we need to:
-- 1) Wrap it in Type var binding.
-- 2) Convert any subsequent instances of Unknown of given type var to BoundTypeVar.
substitute :: Resolutions -> Type -> Type
substitute = go []
    where
        go tv _ (Unknown uid) | uid `elem` tv = BoundTypeVar uid
        go tv subst t@(Unknown uid) =
            case M.lookup uid subst of
                Just t' -> --substitute subst t'  -- Follow chains
                    -- We may need to insert type vars
                    if null [u | u@(Unknown _ ) <- universeBi t']
                        -- no nested unknowns - we may quit
                        then t'
                        -- nested unknowns - wrap in type var
                        else TypeVarBinding uid (go (uid:tv) subst t')
                Nothing -> t
        go tv subst (Ptr t) = Ptr (go tv subst t)
        go tv subst (Fun args ret) =
            Fun (map (go tv subst) args) (go tv subst ret)
        go tv subst (Record fields) = Record [(name, go tv subst t) | (name, t) <- fields]
        go _ _ t = t
