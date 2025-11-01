module Analysis.Typecheck.Typecheck where
import Data.Data (Data)
import Parse.AST
import Analysis.Typecheck.Type
import Control.Monad.State (StateT (runStateT), gets, State, modify)
import Control.Monad.Identity (runIdentity)
import Data.Maybe
import qualified Data.Map as M
import Analysis.Typecheck.Constraints
import Analysis.Typecheck.ConstraintSolver (solve)

data TypeState = TypeState {
    nextId :: Int,
    -- function x local -> ID
    locals :: M.Map (Identifier, Identifier) Type
}

emptyState :: TypeState
emptyState = TypeState 0 M.empty

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

    -- Solve the constraints
    let sol = solve (concat cx)
    case sol of
        Left e -> [e]
        _ -> []

genConstraintsFun :: (Show a, Data a) => FunDecl a -> State TypeState (Constraints a)
genConstraintsFun fun = do
        argC <- genArgs
        retT <- newType
        let argsT = snd <$> argC

        let funConstraints =
                [
                    (CFun (show fun.d) fun, Fun argsT retT),
                    (CExpr (show fun.body.d) fun.body.return, retT)
                ] <> argC

        bodyConstraints <- concat <$> traverse (genConstraintsStmt fun) fun.body.body
        retConstraints <- genConstraintsExpr fun fun.body.return

        pure $ funConstraints <> bodyConstraints <> retConstraints
    where
        genArgs :: State TypeState (Constraints a)
        genArgs = do
            newTypes <- traverse (const newType) fun.args
            pure $ zip (CId (show fun.d) <$> fun.args) newTypes


genConstraintsStmt :: (Show a, Data a) => FunDecl a -> Stmt a -> State TypeState (Constraints a)

genConstraintsStmt f (OutputStmt _ e) = genConstraintsExpr f e

genConstraintsStmt f (WhileStmt l cond body) = do
        bodyC <- genConstraintsStmt f body
        pure $ (CExpr (show l) cond, Int) : bodyC

genConstraintsStmt f (IfStmt l cond truB falsB) = do
        truC <- genConstraintsStmt f truB
        falsC <- fromMaybe [] <$> traverse (genConstraintsStmt f) falsB
        pure $ (CExpr (show l) cond, Int) : (truC <> falsC)

genConstraintsStmt f (Block _ stmtx) = concat <$> traverse (genConstraintsStmt f) stmtx

genConstraintsStmt f (AssignmentStmt l lhs rhs) = do
    -- lhs type must be equal to rhs type
    lhsC <- genConstraintsExpr f lhs
    rhsC <- genConstraintsExpr f rhs
    commonType <- newType
    pure $ (CExpr (show l) lhs, commonType) : (CExpr (show l) rhs, commonType) : (lhsC <> rhsC)

genConstraintsExpr :: (Show a, Data a) => FunDecl a -> Expr a -> State TypeState (Constraints a)

genConstraintsExpr f e@(BiOp l Eq lhs rhs) = do
    -- lhs, rhs must have equal type; we have type int
    lhsC <- genConstraintsExpr f lhs
    rhsC <- genConstraintsExpr f rhs
    commonType <- newType

    pure $ (CExpr (show l) e, Int) : (CExpr (show l) lhs, commonType) : (CExpr (show l) rhs, commonType) : (lhsC <> rhsC)

-- gt, plus, minus, mul, div -- all require ints
genConstraintsExpr f e@(BiOp l _ lhs rhs) = do
    lhsC <- genConstraintsExpr f lhs
    rhsC <- genConstraintsExpr f rhs
    
    pure $ (CExpr (show l) e, Int) : (CExpr (show l) lhs, Int) : (CExpr (show l) rhs, Int) : (lhsC <> rhsC)

genConstraintsExpr f e@(UnOp l Deref target) = do
    targetC <- genConstraintsExpr f target
    underlyingTargetT <- newType

    pure $ (CExpr (show l) target, Ptr underlyingTargetT) : (CExpr (show l) e, underlyingTargetT) : targetC

genConstraintsExpr f e@(UnOp l Ref target) = do
    targetC <- genConstraintsExpr f target
    targetT <- newType

    pure $ (CExpr (show l) target, targetT) : (CExpr (show l) e, Ptr targetT) : targetC

genConstraintsExpr f e@(UnOp l Alloc target) = do
    targetC <- genConstraintsExpr f target
    targetT <- newType

    pure $ (CExpr (show l) target, Ptr targetT) : (CExpr (show l) e, Ptr targetT) : targetC

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
        (CExpr (show l) target, Fun argsTx retT),
        (CExpr (show l) e, retT)
        ] <> targetC <> concat argsCx

genConstraintsExpr _ e@(Parse.AST.Record _ _) = undefined

genConstraintsExpr _ e@(Number l _) = pure [(CExpr (show l) e, Int)]

genConstraintsExpr f e@(EIdentifier l name) = do
    typ <- varType f name
    pure [(CExpr (show l) e, typ)]
