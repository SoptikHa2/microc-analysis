module Analysis.Typecheck.Typecheck where
import Data.Data (Data)
import Parse.AST
import Analysis.Typecheck.Type
import Control.Monad.State (StateT (runStateT), gets, put, State, modify)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Maybe
import qualified Data.Map as M

data Typeable a
    = CExpr (Expr a)
    | CFun (FunDecl a)
    | CId Identifier
    deriving (Show, Eq)

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

type Constraints a = [(Typeable a, Type)]

verify :: (Show a, Data a) => [FunDecl a] -> [TypeError]
verify funcs = do
    -- Generate constraints per function
    let (cx, _state) = runIdentity (runStateT (traverse genConstraintsFun funcs) emptyState)

    show <$> concat cx

genConstraintsFun :: (Show a, Data a) => FunDecl a -> State TypeState (Constraints a)
genConstraintsFun fun = do
        argC <- genArgs
        retT <- newType
        let argsT = snd <$> argC

        let funConstraints =
                [
                    (CFun fun, Fun argsT retT),
                    (CExpr fun.body.return, retT)
                ] <> argC

        bodyConstraints <- concat <$> traverse (genConstraintsStmt fun) fun.body.body
        retConstraints <- genConstraintsExpr fun fun.body.return

        pure $ funConstraints <> bodyConstraints <> retConstraints
    where
        genArgs :: State TypeState (Constraints a)
        genArgs = do
            newTypes <- traverse (const newType) fun.args
            pure $ zip (CId <$> fun.args) newTypes


genConstraintsStmt :: (Show a, Data a) => FunDecl a -> Stmt a -> State TypeState (Constraints a)

genConstraintsStmt f (OutputStmt _ e) = genConstraintsExpr f e

genConstraintsStmt f (WhileStmt _ cond body) = do
        bodyC <- genConstraintsStmt f body
        pure $ (CExpr cond, Int) : bodyC

genConstraintsStmt f (IfStmt _ cond truB falsB) = do
        truC <- genConstraintsStmt f truB
        falsC <- fromMaybe [] <$> traverse (genConstraintsStmt f) falsB
        pure $ (CExpr cond, Int) : (truC <> falsC)

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

    pure $ (CExpr target, Ptr targetT) : (CExpr e, Ptr targetT) : targetC

-- input only works for integers
genConstraintsExpr _ e@(Input _) = pure [(CExpr e, Int)]

genConstraintsExpr _ e@(Null _) = do
    whatever <- newType
    pure [(CExpr e, Ptr whatever)]

genConstraintsExpr _ e@(FieldAccess _ _ _) = undefined

genConstraintsExpr f e@(Call _ target args) = do
    targetC <- genConstraintsExpr f target
    retT <- newType

    argsCx <- traverse (genConstraintsExpr f) args
    argsTx <- traverse (const newType) args

    pure $ [
        (CExpr target, Fun argsTx retT),
        (CExpr e, retT)
        ] <> targetC <> concat argsCx

genConstraintsExpr _ e@(Parse.AST.Record _ _) = undefined

genConstraintsExpr _ e@(Number _ _) = pure [(CExpr e, Int)]

genConstraintsExpr f e@(EIdentifier _ name) = do
    typ <- varType f name
    pure [(CExpr e, typ)]
