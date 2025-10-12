module Analysis.Semantics (SemanticError(..), verify) where
import Parse.AST
import Data.List (nub)
import Analysis.Utils (getIdentifiersUsed, dups)

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
            (verifyRefTaking <$> funcs) <>
            (verifyAssignments <$> funcs) <>
            (verifyFields <$> funcs)
        )
    where
        globals = name <$> funcs

-- Given list of globals (functions), verify the given function is ok -- no dups and no unknowns
verifyIdentifiers :: [Identifier] -> FunDecl -> [SemanticError]
verifyIdentifiers globals fun = notValidErrors <> dupErrors
    where
        validIds = globals <> fun.args <> fun.body.idDecl
        usedIds = getIdentifiersUsed fun.body

        notValidIds = [i | i <- usedIds, i `notElem` validIds]
        dupIds = dups validIds

        notValidErrors = 
            (\i -> eFromFun fun
                (UndeclaredIdentifier $ i <> " is not declared.")) <$> notValidIds
        
        dupErrors =
            (\i -> eFromFun fun
                (DuplicateIdentifier $ i <> " is duplicate.")) <$> dupIds

verifyRefTaking :: FunDecl -> [SemanticError]
verifyRefTaking fun = undefined

verifyAssignments :: FunDecl -> [SemanticError]
verifyAssignments fun = undefined

verifyFields :: FunDecl -> [SemanticError]
verifyFields fun = undefined