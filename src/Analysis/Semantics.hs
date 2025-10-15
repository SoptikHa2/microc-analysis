module Analysis.Semantics (SemanticError(..), verify) where
import Parse.AST
import Data.Generics.Uniplate.Data
import Data.Data
import Data.List (group, sort)
import Debug.Trace (trace)

data SemanticError = UndeclaredIdentifier String
                   | DuplicateIdentifier String
                   | TakingAddrOfFun String
                   | InvalidAssignment String
                   | InvalidRecordField String
                   | NestedRecord String
    deriving (Show)

eFromFun :: FunDecl -> SemanticError -> SemanticError
eFromFun fun = prepend ("In " <> fun.name <> ": ")
    where prepend prefix err = case err of
            UndeclaredIdentifier s -> UndeclaredIdentifier (prefix ++ s)
            DuplicateIdentifier s -> DuplicateIdentifier (prefix ++ s)
            TakingAddrOfFun s -> TakingAddrOfFun (prefix ++ s)
            InvalidAssignment s -> InvalidAssignment (prefix ++ s)
            InvalidRecordField s -> InvalidRecordField (prefix ++ s)
            NestedRecord s -> NestedRecord (prefix ++ s)

verify :: [FunDecl] -> [SemanticError]
verify funcs = concat (
            (verifyIdentifiers globals <$> funcs) <>
            (verifyRefTaking globals <$> funcs) <>
            (verifyAssignments globals <$> funcs) <>
            (verifyFieldDefitions <$> funcs)
        )
    where
        globals = name <$> funcs

-- Given list of globals (functions), verify the given function is ok -- no dups and no unknowns
verifyIdentifiers :: [Identifier] -> FunDecl -> [SemanticError]
verifyIdentifiers globals fun = notValidErrors <> dupErrors
    where
        validIds = globals <> fun.args <> fun.body.idDecl
        usedIds = [i | EIdentifier i <- universeBi fun.body]

        notValidIds = [i | i <- usedIds, i `notElem` validIds]
        dupIds = head <$> filter ((>1) . length) (group $ sort validIds)

        notValidErrors =
            (\i -> eFromFun fun
                (UndeclaredIdentifier $ i <> " is not declared.")) <$> notValidIds

        dupErrors =
            (\i -> eFromFun fun
                (DuplicateIdentifier $ i <> " is duplicate.")) <$> dupIds

-- Given a list of globals (functions), verify that one cannot take ref of a function
verifyRefTaking :: [Identifier] -> FunDecl -> [SemanticError]
verifyRefTaking globals fun = funRefTakenErrors
    where
        idsTaken :: Data a => a -> [Identifier]
        idsTaken node = [i | UnOp Ref (EIdentifier i) <- universeBi node]

        funRefTakenErrors =
            (\i -> eFromFun fun
                (TakingAddrOfFun $ "Taking address of function " <> i)) <$> filter (`elem` globals) (idsTaken fun)

verifyAssignments :: [Identifier] -> FunDecl -> [SemanticError]
verifyAssignments globals fun = assignmentErrors
    where
        -- figure whether assignments are valid
        assignmentTargetValid :: Expr -> Bool
        -- *foo = _ is valid
        assignmentTargetValid (UnOp Deref _) = True
        -- non_global_var = _ is valid
        assignmentTargetValid (EIdentifier id) | id `notElem` globals = True
        -- foo.bar = _ is valid
        assignmentTargetValid (FieldAccess _ _) = True
        -- anything else is invalid
        assignmentTargetValid _ = False

        assignmentsValid :: Data a => a -> [(Expr, Bool)]
        assignmentsValid node = [(target, assignmentTargetValid target) | AssignmentStmt target _ <- universeBi node]

        invalidExprs = fst <$> filter (not . snd) (assignmentsValid fun)

        assignmentErrors =
            (\e -> eFromFun fun
                (InvalidAssignment $ "Cannot assign into non-lvalue: " <> show e)) <$> invalidExprs



-- Record definitions may NOT contain other fields inside them
verifyFieldDefitions :: FunDecl -> [SemanticError]
verifyFieldDefitions fun = errors
    where
        fieldDefs = [f | Record (Fields f) <- universeBi fun]

        fieldDefsWithRecord =
            filter
                ((\e -> case e of Record _ -> True; _ -> False) . snd)
                (concat fieldDefs)
        
        errors =
            (\e -> eFromFun fun
                (NestedRecord $ "Field " <> fst e <> " contains nested field."))
            <$> fieldDefsWithRecord


-- When using a record, one may NOT reference a field it was not declared with
verifyFieldAccess :: FunDecl -> [SemanticError]
verifyFieldAccess fun = []
