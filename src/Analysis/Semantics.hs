{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
module Analysis.Semantics (SemanticError(..), verify, verifyM) where
import Prelude hiding (id)
import Parse.AST hiding (target)
import Data.Generics.Uniplate.Data
import Data.List (group, sort)
import Data.Maybe (catMaybes)
import Data.Data

data SemanticError = UndeclaredIdentifier String
                   | UninitIdentifier String
                   | DuplicateIdentifier String
                   | TakingAddrOfFun String
                   | InvalidAssignment String
                   | InvalidRecordField String
                   | NestedRecord String

instance Show SemanticError where
  show :: SemanticError -> String
  show (UndeclaredIdentifier reason) = "Undeclared identifier: " <> reason
  show (UninitIdentifier reason) = "Uninitialized identifier: " <> reason
  show (DuplicateIdentifier reason) = "Duplicate identifier: " <> reason
  show (TakingAddrOfFun reason) = "Invalid reference: " <> reason
  show (InvalidAssignment reason) = "Bad assignment: " <> reason
  show (InvalidRecordField reason) = "Bad access: " <> reason
  show (NestedRecord reason) = "Bad definition: " <> reason


eFromFun :: Show a => FunDecl a -> Maybe a -> SemanticError -> SemanticError
eFromFun fun loc = case loc of
        Just l -> prepend ("In " <> fun.name <> ", " <> show l <> ": ")
        Nothing -> prepend ("In " <> fun.name <>": ")
    where prepend prefix err = case err of
            UndeclaredIdentifier s -> UndeclaredIdentifier (prefix ++ s)
            UninitIdentifier s -> UninitIdentifier (prefix ++ s)
            DuplicateIdentifier s -> DuplicateIdentifier (prefix ++ s)
            TakingAddrOfFun s -> TakingAddrOfFun (prefix ++ s)
            InvalidAssignment s -> InvalidAssignment (prefix ++ s)
            InvalidRecordField s -> InvalidRecordField (prefix ++ s)
            NestedRecord s -> NestedRecord (prefix ++ s)

verifyM :: (Show a, Data a) => [FunDecl a] -> Either [SemanticError] ()
verifyM = assertEmpty . verify
    where
        assertEmpty [] = Right ()
        assertEmpty xs = Left xs

verify :: (Show a, Data a) => [FunDecl a] -> [SemanticError]
verify funcs = concat (
            (verifyIdentifiers globals <$> funcs) <>
            (verifyInitIdentifiers <$> funcs) <>
            (verifyRefTaking globals <$> funcs) <>
            (verifyAssignments globals <$> funcs) <>
            (verifyFieldDefitions <$> funcs) <>
            (verifyFieldAccess funcs <$> funcs)
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

-- Given a function, verify that all functions that are declared are assigned to before they are used.
verifyInitIdentifiers :: forall a . (Show a, Data a ) => FunDecl a -> [SemanticError]
verifyInitIdentifiers fun = notInitErrors
    where
        -- First of all, get list of stuff to check.
        -- We are only interested in variables that are declared locals in the function.
        locals = fun.body.idDecl

        -- Now, we traverse the tree
        -- node, list of init. identifiers, and list of incorrect uses
        te :: Expr a -> [Identifier] -> [(Identifier, a)]
        te e ix = [(id, loc) | EIdentifier loc id <- universeBi e, id `elem` locals && id `notElem` ix]

        ts :: Stmt a -> [Identifier] -> ([(Identifier, a)], [Identifier])
        ts (OutputStmt _ e) ix = (te e ix, ix)
        ts (WhileStmt _ cond loopBody) ix =
            let
                ce = te cond ix
                (be, bx) = ts loopBody ix
            in
                (ce <> be, bx)
        ts (IfStmt _ c b e) ix =
            let
                ce = te c ix
                (be, bx) = ts b ix
                (ee, ex) = maybe ([], ix) (`ts` ix) e
            in
                (ce <> be <> ee, bx <> ex)
        ts (Block _ []) ix = ([], ix)
        ts (Block l (st:stx)) ix =
            let
                (se, sx) = ts st ix
                (re, rx) = ts (Block l stx) sx
            in
                (se <> re, rx)
        ts (AssignmentStmt _ (EIdentifier _ target) rhs) ix =
            let
                rhse = te rhs ix
            in
                (rhse, target : ix)
        ts (AssignmentStmt _ lhs rhs) ix =
            let
                lhse = te lhs ix
                rhse = te rhs ix
            in
                (lhse <> rhse, ix)
        
        (errors, inited) = ts (Block fun.d fun.body.body) []
        retErrors = te fun.body.return inited

        notInitErrors =
            (\(i, loc) -> eFromFun fun (Just loc)
                (UninitIdentifier $ "Identifier " <> i <> " was not initialized.")) <$> (errors <> retErrors)



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
        -- foo[x] = _ is valid
        assignmentTargetValid (ArrayAccess _ _ _) = True
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

        simpleRecordVars = [id | AssignmentStmt (_ :: a) (EIdentifier (_ :: a) id) (Record _ _) <- universeBi fun]

        fieldDefsWithRecord = catMaybes $ map
            (\(id, e) -> case e of Record loc _ -> Just (id, loc); _ -> Nothing)
            fieldDefs

        fieldDefsWithBadName = catMaybes $ map
            (\(id, e) -> case e of EIdentifier loc i | i `elem` simpleRecordVars -> Just (id, loc); _ -> Nothing)
            fieldDefs

        errors =
            (\(id, loc) -> eFromFun fun (Just loc)
                (NestedRecord $ "Field " <> id <> " contains nested field."))
            <$> (fieldDefsWithRecord <> fieldDefsWithBadName)


-- When using a record, one may NOT reference a field it was not declared with
verifyFieldAccess :: forall a. (Show a, Data a) => [FunDecl a] -> FunDecl a -> [SemanticError]
verifyFieldAccess allFuncs fun = errors
    where
        fieldDefs = concat [f | Record (_ :: a) (Fields f) <- universeBi allFuncs]

        fieldNames = fst <$> fieldDefs

        fieldAccesses = [(i, loc) | FieldAccess loc _ i <- universeBi fun]

        invalidAccess = filter (\(e, _) -> e `notElem` fieldNames) fieldAccesses

        errors =
            (\(e, loc) -> eFromFun fun (Just loc)
                (InvalidRecordField $ "Accessing unknown record field: " <> e))
            <$> invalidAccess

