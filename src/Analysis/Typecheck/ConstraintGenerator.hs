module Analysis.Typecheck.ConstraintGenerator where
import Data.Data (Data)
import Parse.AST
import Control.Monad.State
import Analysis.Typecheck.Constraints
import qualified Data.Map as M
import Analysis.Typecheck.Type
import Data.Maybe (fromMaybe)
import qualified Analysis.Typecheck.Type as Type


data TypeState = TypeState {
    nextId :: Int,
    -- function x local -> ID
    locals :: M.Map (Identifier, Identifier) Type,
    allFieldNames :: [Identifier]
}

emptyState :: [Identifier] -> TypeState
emptyState = TypeState 0 M.empty

-- Generate new unknown type
newType :: State TypeState Type
newType = do
    ni <- gets nextId
    modify (\(TypeState _ locals afs) -> TypeState (ni + 1) locals afs)
    pure $ Unknown ni

varType :: FunDecl a -> Identifier -> State TypeState Type
varType fun var = do
    locals <- gets locals
    let key = (fun.name, var)
    let val = M.lookup key locals
    case val of
        Just existingType -> pure existingType
        Nothing -> do
            nt <- newType
            modify (\(TypeState ni locals afs) -> TypeState ni (M.insert key nt locals) afs)
            pure nt

genConstraintsFun :: (Show a, Data a) => FunDecl a -> State TypeState (Constraints a)
genConstraintsFun fun = do
        argC <- genArgs
        retT <- newType
        let argsT = snd <$> argC

        let funConstraints =
                [
                    (CFun (show fun.d) fun, Fun argsT retT),
                    (CExpr (show $ exprLoc fun.body.return) fun.body.return, retT)
                ] <> argC

        bodyConstraints <- concat <$> traverse (genConstraintsStmt fun) fun.body.body
        retConstraints <- genConstraintsExpr fun fun.body.return

        -- Add CId constraints for all local variables (not just parameters)
        -- This ensures each variable has exactly one CId entry for deduplication
        localsMap <- gets locals
        let localConstraints = [(CId (fun.name ++ ":" ++ show fun.d) varName, typ)
                               | ((funName, varName), typ) <- M.toList localsMap
                               , funName == fun.name
                               , varName `notElem` fun.args]  -- Skip parameters (already in argC)

        pure $ funConstraints <> bodyConstraints <> retConstraints <> localConstraints
    where
        genArgs :: State TypeState (Constraints a)
        genArgs = do
            newTypes <- traverse (const newType) fun.args
            pure $ zip (CId (show fun.d) <$> fun.args) newTypes


genConstraintsStmt :: (Show a, Data a) => FunDecl a -> Stmt a -> State TypeState (Constraints a)

genConstraintsStmt f (OutputStmt _ e) = genConstraintsExpr f e

genConstraintsStmt f (WhileStmt _ cond body) = do
        bodyC <- genConstraintsStmt f body
        pure $ (CExpr (show $ exprLoc cond) cond, Int) : bodyC

genConstraintsStmt f (IfStmt _ cond truB falsB) = do
        truC <- genConstraintsStmt f truB
        falsC <- fromMaybe [] <$> traverse (genConstraintsStmt f) falsB
        pure $ (CExpr (show $ exprLoc cond) cond, Int) : (truC <> falsC)

genConstraintsStmt f (Block _ stmtx) = concat <$> traverse (genConstraintsStmt f) stmtx

genConstraintsStmt f (AssignmentStmt _ lhs rhs) = do
    -- lhs type must be equal to rhs type
    lhsC <- genConstraintsExpr f lhs
    rhsC <- genConstraintsExpr f rhs
    commonType <- newType
    pure $ (CExpr (show $ exprLoc lhs) lhs, commonType) : (CExpr (show $ exprLoc rhs) rhs, commonType) : (lhsC <> rhsC)

genConstraintsExpr :: (Show a, Data a) => FunDecl a -> Expr a -> State TypeState (Constraints a)

genConstraintsExpr f e@(BiOp l Eq lhs rhs) = do
    -- lhs, rhs must have equal type; we have type int
    lhsC <- genConstraintsExpr f lhs
    rhsC <- genConstraintsExpr f rhs
    commonType <- newType

    pure $ (CExpr (show l) e, Int) : (CExpr (show $ exprLoc lhs) lhs, commonType) : (CExpr (show $ exprLoc rhs) rhs, commonType) : (lhsC <> rhsC)

-- gt, plus, minus, mul, div -- all require ints
genConstraintsExpr f e@(BiOp l _ lhs rhs) = do
    lhsC <- genConstraintsExpr f lhs
    rhsC <- genConstraintsExpr f rhs

    pure $ (CExpr (show l) e, Int) : (CExpr (show $ exprLoc lhs) lhs, Int) : (CExpr (show $ exprLoc rhs) rhs, Int) : (lhsC <> rhsC)

genConstraintsExpr f e@(UnOp l Deref target) = do
    targetC <- genConstraintsExpr f target
    underlyingTargetT <- newType

    pure $ (CExpr (show $ exprLoc target) target, Ptr underlyingTargetT) : (CExpr (show l) e, underlyingTargetT) : targetC

genConstraintsExpr f e@(UnOp l Ref target) = do
    targetC <- genConstraintsExpr f target
    targetT <- newType

    pure $ (CExpr (show $ exprLoc target) target, targetT) : (CExpr (show l) e, Ptr targetT) : targetC

genConstraintsExpr f e@(UnOp l Alloc target) = do
    targetC <- genConstraintsExpr f target
    targetT <- newType

    pure $ (CExpr (show $ exprLoc target) target, targetT) : (CExpr (show l) e, Ptr targetT) : targetC

-- input only works for integers
genConstraintsExpr _ e@(Input l) = pure [(CExpr (show l) e, Int)]

genConstraintsExpr _ e@(Null l) = do
    whatever <- newType
    pure [(CExpr (show l) e, Ptr whatever)]

genConstraintsExpr f e@(FieldAccess _ target field) = do
    -- Target has to be record type that contains current field
    -- with type equal to the result of this expression
    fieldType <- newType
    targetT <- genConstraintsExpr f target
    pure $ [
        (CExpr (show $ exprLoc e) e, fieldType),
        (CExpr (show $ exprLoc target) target, 
            Type.Record [(field, fieldType)])
        ] <> targetT

genConstraintsExpr f e@(Call l target args) = do
    targetC <- genConstraintsExpr f target
    retT <- newType

    argsCx <- traverse (genConstraintsExpr f) args
    argsTx <- traverse (const newType) args
    let argConstraints = zipWith (\arg argT -> (CExpr (show $ exprLoc arg) arg, argT)) args argsTx

    pure $ [
        (CExpr (show $ exprLoc target) target, Fun argsTx retT),
        (CExpr (show l) e, retT)
        ] <> argConstraints <> targetC <> concat argsCx

genConstraintsExpr f e@(Parse.AST.Record l (Fields fields)) = do
    -- This is a record constructor/definition. Generate new unknown for each field.
    fieldTypes <- traverse (const newType) fields
    let recTypes = zip (fst <$> fields) fieldTypes
    -- We need to create bottom types for fields not explicitly mentioned
    allFields <- gets allFieldNames
    let extraFields = filter (`notElem` allFields) (fst <$> fields)
    let extraFieldsTyping = (\n -> (n, Bottom)) <$> extraFields
    let recordType = Type.Record $ recTypes <> extraFieldsTyping
    -- Generate specific types per the expression
    let exprTypes = zip (snd <$> fields) fieldTypes
    let typeableTypes = (\(e, t) -> (CExpr (show $ exprLoc e) e, t)) <$> exprTypes
    -- Generate typing for the nested expressions
    nestedCtx <- traverse (genConstraintsExpr f) (snd <$> fields)

    pure $ [
        (CExpr (show l) e, recordType)
        ] <> typeableTypes <> concat nestedCtx

genConstraintsExpr _ e@(Number l _) = pure [(CExpr (show l) e, Int)]

genConstraintsExpr f e@(EIdentifier l name) = do
    typ <- varType f name
    pure [(CExpr (show l) e, typ)]
