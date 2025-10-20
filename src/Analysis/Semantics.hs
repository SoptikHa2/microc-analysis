{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Semantics (SemanticError(..), verify) where
import Parse.AST
import Data.Generics.Uniplate.Data
import Data.List (group, sort)
import Data.Maybe (catMaybes)
import Data.Data
import Debug.Trace (trace)

data SemanticError = UndeclaredIdentifier String
                   | DuplicateIdentifier String
                   | TakingAddrOfFun String
                   | InvalidAssignment String
                   | InvalidRecordField String
                   | NestedRecord String
    deriving (Show)

eFromFun :: Show a => FunDecl a -> Maybe a -> SemanticError -> SemanticError
eFromFun fun loc = case loc of
        Just loc -> prepend ("In " <> fun.name <> ", " <> show loc <> ": ")
        Nothing -> prepend ("In " <> fun.name <>": ")
    where prepend prefix err = case err of
            UndeclaredIdentifier s -> UndeclaredIdentifier (prefix ++ s)
            DuplicateIdentifier s -> DuplicateIdentifier (prefix ++ s)
            TakingAddrOfFun s -> TakingAddrOfFun (prefix ++ s)
            InvalidAssignment s -> InvalidAssignment (prefix ++ s)
            InvalidRecordField s -> InvalidRecordField (prefix ++ s)
            NestedRecord s -> NestedRecord (prefix ++ s)

verify :: (Show a, Data a) => [FunDecl a] -> [SemanticError]
verify funcs = concat (
            (verifyIdentifiers globals <$> funcs) <>
            (verifyRefTaking globals <$> funcs) <>
            (verifyAssignments globals <$> funcs) <>
            (verifyFieldDefitions <$> funcs) <>
            (verifyFieldAccess <$> funcs)
        )
    where
        globals = name <$> funcs

-- Given list of globals (functions), verify the given function is ok -- no dups and no unknowns
verifyIdentifiers :: (Show a, Data a) => [Identifier] -> FunDecl a -> [SemanticError]
verifyIdentifiers globals fun = notValidErrors <> dupErrors
    where
        validIds = globals <> fun.args <> fun.body.idDecl
        usedIds = [(i, loc) | EIdentifier loc i <- universeBi fun.body]

        notValidIds = [(i, loc) | (i, loc) <- usedIds, i `notElem` validIds]
        dupIds = head <$> filter ((>1) . length) (group $ sort validIds)

        notValidErrors =
            (\(i, loc) -> eFromFun fun (Just loc)
                (UndeclaredIdentifier $ i <> " is not declared.")) <$> notValidIds

        dupErrors =
            (\i -> eFromFun fun Nothing
                (DuplicateIdentifier $ i <> " is duplicate.")) <$> dupIds

-- Given a list of globals (functions), verify that one cannot take ref of a function
verifyRefTaking :: (Show a, Data a) => [Identifier] -> FunDecl a -> [SemanticError]
verifyRefTaking globals fun = funRefTakenErrors
    where
        idsTaken node = [(i, loc) | UnOp loc Ref (EIdentifier _ i) <- universeBi node]

        funRefTakenErrors =
            (\(i, loc) -> eFromFun fun (Just loc)
                (TakingAddrOfFun $ "Taking address of function " <> i)) <$> filter (\(e,_) -> e `elem` globals) (idsTaken fun)

verifyAssignments :: (Show a, Data a) => [Identifier] -> FunDecl a -> [SemanticError]
verifyAssignments globals fun = assignmentErrors
    where
        -- figure whether assignments are valid
        assignmentTargetValid :: Expr a -> Bool
        -- *foo = _ is valid
        assignmentTargetValid (UnOp _ Deref _) = True
        -- non_global_var = _ is valid
        assignmentTargetValid (EIdentifier _ id) | id `notElem` globals = True
        -- foo.bar = _ is valid
        assignmentTargetValid (FieldAccess _ _ _) = True
        -- anything else is invalid
        assignmentTargetValid _ = False

        assignmentsValid node = [((target, loc), assignmentTargetValid target) | AssignmentStmt loc target _ <- universeBi node]

        invalidExprs = fst <$> filter (not . snd) (assignmentsValid fun)

        assignmentErrors =
            (\(e, loc) -> eFromFun fun (Just loc)
                (InvalidAssignment $ "Cannot assign into non-lvalue: " <> show e)) <$> invalidExprs



-- Record definitions may NOT contain other fields inside them
verifyFieldDefitions :: forall a. (Show a, Data a) => FunDecl a -> [SemanticError]
verifyFieldDefitions fun = errors
    where
        fieldDefs = concat [f | Record (_ :: a) (Fields f) <- universeBi fun]

        fieldDefsWithRecord = catMaybes $ map
            (\(id, e) -> case e of Record loc _ -> Just (id, loc); _ -> Nothing)
            fieldDefs
        
        errors =
            (\(id, loc) -> eFromFun fun (Just loc)
                (NestedRecord $ "Field " <> id <> " contains nested field."))
            <$> fieldDefsWithRecord


-- When using a record, one may NOT reference a field it was not declared with
verifyFieldAccess :: forall a. (Show a, Data a) => FunDecl a -> [SemanticError]
verifyFieldAccess fun = errors
    where
        fieldDefs = concat [f | Record (_ :: a) (Fields f) <- universeBi fun]

        fieldNames = fst <$> fieldDefs

        fieldAccesses = [(i, loc) | FieldAccess loc _ i <- universeBi fun]
        
        invalidAccess = filter (\(e, _) -> e `notElem` fieldNames) fieldAccesses

        errors =
            (\(e, loc) -> eFromFun fun (Just loc)
                (InvalidRecordField $ "Accessing unknown record field: " <> e))
            <$> invalidAccess

