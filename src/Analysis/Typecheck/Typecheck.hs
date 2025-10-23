module Analysis.Typecheck.Typecheck where
import Data.Data (Data)
import Parse.AST
import Analysis.Typecheck.Type
import Control.Monad.State (StateT (runStateT), gets, put)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Maybe

data Typeable a
    = CExpr (Expr a)
    | CFun (FunDecl a)
    | CId Identifier
    deriving (Show, Eq)

type State = Int

-- Generate new unknown type
newType :: StateT State Identity Type
newType = do
    ni <- gets id
    put (ni + 1)
    pure $ Unknown ni

type Constraints a = [(Typeable a, Type)]

verify :: (Show a, Data a) => [FunDecl a] -> [TypeError]
verify funcs = do
    -- Generate constraints per function
    let cxState = runIdentity $ traverse (\fun -> runStateT (genConstraintsFun fun) 0) funcs
    let cx = concatMap fst cxState

    show <$> cx


genConstraintsFun :: (Show a, Data a) => FunDecl a -> StateT State Identity (Constraints a)
genConstraintsFun fun = do
        argC <- genArgs
        retT <- newType
        let argsT = snd <$> argC

        let funConstraints =
                [
                    (CFun fun, Fun argsT retT),
                    (CExpr fun.body.return, retT)
                ] <> argC

        bodyConstraints <- concat <$> traverse genConstraintsStmt fun.body.body
        retConstraints <- genConstraintsExpr fun.body.return

        pure $ funConstraints <> bodyConstraints <> retConstraints
    where
        genArgs :: StateT State Identity (Constraints a)
        genArgs = do
            newTypes <- traverse (const newType) fun.args
            pure $ zip (CId <$> fun.args) newTypes


genConstraintsStmt :: (Show a, Data a) => Stmt a -> StateT State Identity (Constraints a)

genConstraintsStmt (OutputStmt _ e) = genConstraintsExpr e

genConstraintsStmt (WhileStmt _ cond body) = do
        bodyC <- genConstraintsStmt body
        pure $ (CExpr cond, Int) : bodyC

genConstraintsStmt  (IfStmt _ cond truB falsB) = do
        truC <- genConstraintsStmt truB
        falsC <- fromMaybe [] <$> traverse genConstraintsStmt falsB
        pure $ (CExpr cond, Int) : (truC <> falsC)

genConstraintsStmt (Block _ stmtx) = concat <$> traverse genConstraintsStmt stmtx

genConstraintsStmt (AssignmentStmt _ lhs rhs) = do
    -- lhs type must be equal to rhs type
    lhsC <- genConstraintsExpr lhs
    rhsC <- genConstraintsExpr rhs
    commonType <- newType
    pure $ (CExpr lhs, commonType) : (CExpr rhs, commonType) : (lhsC <> rhsC)

genConstraintsExpr :: (Show a, Data a) => Expr a -> StateT State Identity (Constraints a)

genConstraintsExpr e@(BiOp _ Eq lhs rhs) = do
    -- lhs, rhs must have equal type; we have type int
    lhsC <- genConstraintsExpr lhs
    rhsC <- genConstraintsExpr rhs
    commonType <- newType

    pure $ (CExpr e, Int) : (CExpr lhs, commonType) : (CExpr rhs, commonType) : (lhsC <> rhsC)



genConstraintsExpr e@(Number _ _) = pure [(CExpr e, Int)]

genConstraintsExpr (EIdentifier _ _) = pure []

genConstraintsExpr _ = undefined
