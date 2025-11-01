{-# LANGUAGE RankNTypes #-}
module Analysis.Typecheck.Typecheck (verify, getTyping, printTyping) where
import Data.Data (Data)
import Parse.AST
import Analysis.Typecheck.Type
import Control.Monad.State (StateT (runStateT), gets, State, modify)
import Control.Monad.Identity (runIdentity)
import Data.Maybe
import qualified Data.Map as M
import Analysis.Typecheck.Constraints
import Analysis.Typecheck.ConstraintSolver (solve)
import Debug.Trace
import Data.List (intercalate)

data TypeState = TypeState {
    nextId :: Int,
    -- function x local -> ID
    locals :: M.Map (Identifier, Identifier) Type
}

emptyState :: TypeState
emptyState = TypeState 0 M.empty

-- Extract location from any expression
exprLoc :: Expr a -> a
exprLoc (BiOp l _ _ _) = l
exprLoc (UnOp l _ _) = l
exprLoc (Input l) = l
exprLoc (Null l) = l
exprLoc (FieldAccess l _ _) = l
exprLoc (Call l _ _) = l
exprLoc (Parse.AST.Record l _) = l
exprLoc (Number l _) = l
exprLoc (EIdentifier l _) = l

-- Generate new unknown type
newType :: State TypeState Type
newType = do
    ni <- gets nextId
    modify (\(TypeState _ locals) -> TypeState (ni + 1) locals)
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
            modify (\(TypeState ni locals) -> TypeState ni (M.insert key nt locals))
            pure nt


verify :: (Show a, Data a, Ord a) => [FunDecl a] -> [TypeError]
verify funcs = do
    -- Generate constraints per function
    let (cx, _state) = runIdentity (runStateT (traverse genConstraintsFun funcs) emptyState)

    case solve (concat cx) of
        Left te -> [te]
        Right _ -> []

getTyping :: (Show a, Data a, Ord a) => [FunDecl a] -> Either TypeError (M.Map (Typeable a) Type)
getTyping funcs = do
    -- Generate constraints per function
    let (cx, _state) = runIdentity (runStateT (traverse genConstraintsFun funcs) emptyState)

    solve (concat cx)

printTyping :: forall a . (Show a) => (M.Map (Typeable a) Type) -> String
printTyping m = intercalate "\n" (filter (/= "") (M.elems $ M.mapWithKey go m))
    where
        go :: Show a => (Typeable a) -> Type -> String
        go (CExpr _ _) _ = ""
        go (CFun l f) t = "[" ++ f.name ++ "() :: " ++ l ++ "] = " ++ show t
        go (CId l i) t = "[" ++ i ++ " :: " ++ l ++ "] = " ++ show t

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

genConstraintsExpr _ e@(FieldAccess _ _ _) = undefined

genConstraintsExpr f e@(Call l target args) = do
    targetC <- genConstraintsExpr f target
    retT <- newType

    argsCx <- traverse (genConstraintsExpr f) args
    argsTx <- traverse (const newType) args

    pure $ [
        (CExpr (show $ exprLoc target) target, Fun argsTx retT),
        (CExpr (show l) e, retT)
        ] <> targetC <> concat argsCx

genConstraintsExpr _ e@(Parse.AST.Record _ _) = undefined

genConstraintsExpr _ e@(Number l _) = pure [(CExpr (show l) e, Int)]

genConstraintsExpr f e@(EIdentifier l name) = do
    typ <- varType f name
    pure [(CExpr (show l) e, typ)]
