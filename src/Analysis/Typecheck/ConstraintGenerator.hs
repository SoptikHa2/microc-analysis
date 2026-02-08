module Analysis.Typecheck.ConstraintGenerator (TypeState(..), emptyState, newType, varType, registerFunction, lookupFunction, genConstraintsFun, genConstraintsExpr, genConstraintsStmt) where
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
    allFieldNames :: [Identifier],
    -- Global function types: function name -> type
    globalFunctions :: M.Map Identifier Type
}

emptyState :: [Identifier] -> TypeState
emptyState fn = TypeState 0 M.empty fn M.empty

-- Generate new unknown type
newType :: State TypeState Type
newType = do
    ni <- gets nextId
    modify (\s -> s { nextId = ni + 1 })
    pure $ Unknown ni

-- Get or create a type for a local variable
varType :: FunDecl a -> Identifier -> State TypeState Type
varType fun var = do
    localsMap <- gets locals
    let key = (fun.name, var)
    case M.lookup key localsMap of
        Just existingType -> pure existingType
        Nothing -> do
            nt <- newType
            modify (\s -> s { locals = M.insert key nt (locals s) })
            pure nt

-- Register a global function with its type
registerFunction :: Identifier -> Type -> State TypeState ()
registerFunction name typ = do
    modify (\s -> s { globalFunctions = M.insert name typ (globalFunctions s) })

-- Look up a global function type
lookupFunction :: Identifier -> State TypeState (Maybe Type)
lookupFunction name = do
    M.lookup name <$> gets globalFunctions

genConstraintsFun :: (Show a, Data a) => FunDecl a -> State TypeState (Constraints a)
genConstraintsFun fun = do
        argC <- genArgs
        retT <- newType
        let argsT = snd <$> argC
        let funType = Fun argsT retT

        -- Register this function globally so other functions can reference it
        registerFunction fun.name funType

        -- Bind args to variables
        argVarTypes <- traverse (varType fun) fun.args
        let argBinds = zip argTy argVarTypes

        -- If we are main, all args are integers
        let defaultArgBinds = if fun.name == "main"
            then zip argTy (repeat Int)
            else []

        let funConstraints =
                [
                    (CFun fun, funType),
                    (CExpr fun.body.return, retT)
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

        pure $ funConstraints <> bodyConstraints <> retConstraints <> localConstraints <> argBinds <> defaultArgBinds
    where
        argTy = CId (show fun.d) <$> fun.args

        genArgs :: State TypeState (Constraints a)
        genArgs = do
            newTypes <- traverse (const newType) fun.args
            pure $ zip argTy newTypes


genConstraintsStmt :: (Show a, Data a) => FunDecl a -> Stmt a -> State TypeState (Constraints a)

genConstraintsStmt f (OutputStmt _ e) = genConstraintsExpr f e

genConstraintsStmt f (WhileStmt _ cond body) = do
        condC <- genConstraintsExpr f cond
        bodyC <- genConstraintsStmt f body
        pure $ (CExpr cond, Int) : (condC <> bodyC)

genConstraintsStmt f (IfStmt _ cond truB falsB) = do
        condC <- genConstraintsExpr f cond
        truC <- genConstraintsStmt f truB
        falsC <- fromMaybe [] <$> traverse (genConstraintsStmt f) falsB
        pure $ (CExpr cond, Int) : (condC <> truC <> falsC)

genConstraintsStmt f (Block _ stmtx) = concat <$> traverse (genConstraintsStmt f) stmtx

genConstraintsStmt f (AssignmentStmt _ lhs rhs) = do
    -- lhs type must be equal to rhs type
    lhsC <- genConstraintsExpr f lhs
    rhsC <- genConstraintsExpr f rhs
    commonType <- newType
    pure $ (CExpr lhs, commonType) : (CExpr rhs, commonType) : (lhsC <> rhsC)

genConstraintsExpr :: (Show a, Data a) => FunDecl a -> Expr a -> State TypeState (Constraints a)

genConstraintsExpr f e@(BiOp _ Eq lhs rhs) = do
    -- lhs, rhs must have equal type; we have type int
    lhsC <- genConstraintsExpr f lhs
    rhsC <- genConstraintsExpr f rhs
    commonType <- newType

    pure $ (CExpr e, Int) : (CExpr lhs, commonType) : (CExpr rhs, commonType) : (lhsC <> rhsC)

-- gt, plus, minus, mul, div -- all require ints
genConstraintsExpr f e@(BiOp _ _ lhs rhs) = do
    lhsC <- genConstraintsExpr f lhs
    rhsC <- genConstraintsExpr f rhs

    pure $ (CExpr e, Int) : (CExpr lhs, Int) : (CExpr rhs, Int) : (lhsC <> rhsC)

genConstraintsExpr f e@(UnOp _ Deref target) = do
    targetC <- genConstraintsExpr f target
    underlyingTargetT <- newType

    pure $ (CExpr target, Ptr underlyingTargetT) : (CExpr e, underlyingTargetT) : targetC

genConstraintsExpr f e@(UnOp _ Ref target) = do
    targetC <- genConstraintsExpr f target
    targetT <- newType

    pure $ (CExpr target, targetT) : (CExpr e, Ptr targetT) : targetC

genConstraintsExpr f e@(UnOp _ Alloc target) = do
    targetC <- genConstraintsExpr f target
    targetT <- newType

    pure $ (CExpr target, targetT) : (CExpr e, Ptr targetT) : targetC

genConstraintsExpr f e@(UnOp _ Not target) = do
    targetC <- genConstraintsExpr f target
    pure $ (CExpr target, Int) : (CExpr e, Int) : targetC

-- input only works for integers
genConstraintsExpr _ e@(Input _) = pure [(CExpr e, Int)]

genConstraintsExpr _ e@(Null _) = do
    whatever <- newType
    pure [(CExpr e, Ptr whatever)]

genConstraintsExpr f e@(FieldAccess _ target field) = do
    -- Target has to be record type that contains current field
    -- with type equal to the result of this expression.
    -- Except for when the field does not exist, then it is a bottom type.
    allFields <- gets allFieldNames

    fieldType <- if field `elem` allFields
                    then newType
                    else pure Bottom
    targetT <- genConstraintsExpr f target
    let extraFields = filter (`notElem` [field]) allFields
    -- Use fresh type variables for non-accessed fields
    extraFieldTypes <- traverse (const newType) extraFields
    let extraFieldsTyping = zip extraFields extraFieldTypes

    pure $ [
        (CExpr e, fieldType),
        (CExpr target,
            Type.Record ([(field, fieldType)] <> extraFieldsTyping))
        ] <> targetT

genConstraintsExpr f e@(ArrayAccess _ target idx) = do
    -- 1) Index has to be of type integer
    -- 2) Target has to be of type [X]
    -- 3) This expression has to be of type X
    arrayInnerType <- newType
    targetC <- genConstraintsExpr f target
    idxC <- genConstraintsExpr f idx

    pure $ [
        (CExpr idx, Int),
        (CExpr target, Type.Array arrayInnerType),
        (CExpr e, arrayInnerType)
        ] <> targetC <> idxC

genConstraintsExpr f e@(Call _ target args) = do
    targetC <- genConstraintsExpr f target
    retT <- newType

    argsCx <- traverse (genConstraintsExpr f) args
    argsTx <- traverse (const newType) args
    let argConstraints = zipWith (\arg argT -> (CExpr arg, argT)) args argsTx

    pure $ [
        (CExpr target, Fun argsTx retT),
        (CExpr e, retT)
        ] <> argConstraints <> targetC <> concat argsCx

genConstraintsExpr f e@(Parse.AST.Record _ (Fields fields)) = do
    -- This is a record constructor/definition. Generate new unknown for each field.
    fieldTypes <- traverse (const newType) fields
    let recTypes = zip (fst <$> fields) fieldTypes
    -- Use bottom type for all other fields
    allFields <- gets allFieldNames
    let extraFields = filter (`notElem` (fst <$> fields)) allFields
    let extraFieldsTyping = zip extraFields (repeat Bottom)
    let recordType = Type.Record $ recTypes <> extraFieldsTyping
    -- Generate specific types per the expression
    let exprTypes = zip (snd <$> fields) fieldTypes
    let typeableTypes = (\(e, t) -> (CExpr e, t)) <$> exprTypes
    -- Generate typing for the nested expressions
    nestedCtx <- traverse (genConstraintsExpr f) (snd <$> fields)

    pure $ [
        (CExpr e, recordType)
        ] <> typeableTypes <> concat nestedCtx

genConstraintsExpr f e@(Parse.AST.Array _ items) = do
    -- 1) All underlying items have to be of the same type X
    -- 2) This is of type [X]
    itemsC <- traverse (genConstraintsExpr f) items
    innerArrayType <- newType

    let itemTypes = (\e -> (CExpr e, innerArrayType)) <$> items

    pure $ [
        (CExpr e, Type.Array innerArrayType)
        ] <> concat itemsC <> itemTypes

genConstraintsExpr _ e@(Number _ _) = pure [(CExpr e, Int)]

genConstraintsExpr f e@(EIdentifier _ name) = do
    -- Check if it's a global function first
    maybeFunType <- lookupFunction name
    typ <- case maybeFunType of
        Just funType -> pure funType
        Nothing -> varType f name  -- It's a local variable
    pure [(CExpr e, typ)]
