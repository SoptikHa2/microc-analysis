{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Typecheck.ConstraintSolver where
import Analysis.Typecheck.Constraints (Constraints, Typeable, typeableLoc, prettyPrintMTT)
import Analysis.Typecheck.Type (TypeError, Type(..))
import Control.Monad (zipWithM)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Data.Generics.Uniplate.Data (universeBi, transform)

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
                    -- Apply substitutions to all types
                    let tpt' = M.map (map (substitute (bindTypeVars substitutions))) tpt
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
merge _ Bottom t2 = Right t2
merge _ t1 Bottom = Right t1
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
    | otherwise = Left $ l ++ ": Function being called has arity " ++ show (length args1) ++ ", but should have " ++ show (length args2)
merge l (Record fields1) (Record fields2) = do
    mergedFields <- zipWithM mergeField fields1 fields2
    return $ Record mergedFields
  where
    mergeField (n1, t1) (n2, t2)
        | n1 == n2 = (,) n1 <$> merge l t1 t2
        | otherwise = Left $ l ++ ": Field name mismatch: " <> n1 <> " vs " <> n2
merge l t1 t2 = Left $ l ++ ": The type is " <> show t1 <> ", but should be " <> show t2

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

bindTypeVars :: Resolutions -> Resolutions
-- We may have a resolution that looks like: TypeVar 3 binds into -> Ptr (TypeVar 3)
-- This will trigger infinite substitution. In this case, we want to 
-- convert it to TypeVar3 binds into TypeVarBinding 3 (Ptr (BoundTypeVar 3))
bindTypeVars = M.mapWithKey bind
    where
        unknownsInside t = [i | Unknown i <- universeBi t]

        shouldBind :: Int -> Type -> Bool
        shouldBind k v = k `elem` unknowns
            where
                unknowns = unknownsInside v

        bind :: Int -> Type -> Type
        bind k v | not $ shouldBind k v = v
        -- bind
        bind k v = TypeVarBinding k $ transform f v
            where
                f (Unknown i) | i == k = BoundTypeVar i
                f x = x

-- Apply substitutions recursively to a type
substitute :: Resolutions -> Type -> Type
substitute subst t@(Unknown uid) =
    case M.lookup uid subst of
        Just t' -> substitute subst t'  -- Follow chains
        Nothing -> t
substitute subst (Ptr t) = Ptr (substitute subst t)
substitute subst (Fun args ret) =
    Fun (map (substitute subst) args) (substitute subst ret)
substitute subst (Record fields) = Record [(name, substitute subst t) | (name, t) <- fields]
substitute _ t = t


