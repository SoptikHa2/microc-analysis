module Interpreter.Interpret (evalExpr, applyBiOp, evalStmt, evalExprForWrite) where

import Prelude hiding (id)
import qualified Prelude
import Parse.AST
import Interpreter.State
import Interpreter.Data
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.State (StateT, mapStateT, MonadTrans (lift))
import Control.Monad (forM, forM_, when)
import Data.Foldable (traverse_)

runId :: StateT State Identity a -> StateT State IO a
runId = mapStateT (pure . runIdentity)

id' :: a -> a
id' = Prelude.id

applyBiOp :: BiOp -> Value -> Value -> Value
applyBiOp Eq v1 v2 = VNumber $ if v1 == v2 then 1 else 0
applyBiOp Gt (VNumber i1) (VNumber i2) = VNumber $ if i1 > i2 then 1 else 0
applyBiOp Plus (VNumber i1) (VNumber i2) = VNumber $ i1 + i2
applyBiOp Minus (VNumber i1) (VNumber i2) = VNumber $ i1 - i2
applyBiOp Mul (VNumber i1) (VNumber i2) = VNumber $ i1 * i2
applyBiOp Div (VNumber i1) (VNumber i2) = if i2 == 0
    then error "Division by zero"
    else VNumber $ i1 `div` i2
applyBiOp op v1 v2 = error $ "Undefined operation " ++ show op ++ " on " ++ show v1 ++ ", " ++ show v2

evalExpr :: Expr -> StateT State IO Value
evalExpr (BiOp op e1 e2) = do
    ee1 <- evalExpr e1
    ee2 <- evalExpr e2
    pure $ applyBiOp op ee1 ee2

evalExpr (UnOp Deref e1) = do
    ee1 <- evalExpr e1
    case ee1 of
        Pointer addr -> do
            v <- runId $ getsAddr addr
            case v of
                Just value -> pure value
                Nothing -> error $ "Dangling pointer: " ++ show addr
        _ -> error "Attempted to deref non-pointer"

evalExpr (UnOp Ref (EIdentifier id)) = do
    v <- runId $ getsVarAddr id
    case v of
        Just value -> pure $ Pointer value
        Nothing -> error $ "Undefined variable " ++ show id

evalExpr (UnOp Ref _) = error "Attempted to take address of non-variable"

evalExpr (UnOp Alloc e1) = do
    valueToStore <- evalExpr e1
    addr <- runId $ putsValue valueToStore
    pure $ Pointer addr

evalExpr Input = do
    -- Maybe more datatypes?
    num <- lift (readLn :: IO Int)
    pure $ VNumber num

evalExpr Null = pure $ Pointer 0

evalExpr (FieldAccess e id) = do
    fieldStruct <- evalExpr e
    case fieldStruct of
        Interpreter.Data.Record mapping -> do
            let addr = maybe (error $ "Nonexistent field " ++ show id) id' (lookup id mapping)
            runId (getsAddr addr) >>= maybe (error $ "Invalid field " ++ show id) pure
        _ -> error $ "Attempted to access field of " ++ show e

evalExpr (Parse.AST.Record (Fields fx)) = do
    -- Evaluate all the fields
    rr <- forM fx (\(n, value) -> (,) n <$> evalExpr value)
    -- Save them into memory
    ar <- forM rr (\(n, value) -> (,) n <$> runId (putsValue value))
    pure $ Interpreter.Data.Record ar

evalExpr (Number i) = pure $ VNumber i

evalExpr (EIdentifier id) = do
    val <- runId $ getsVar id
    case val of
        Just v -> pure v
        Nothing -> error $ "Unknown variable " ++ id

evalExpr (Call fun params) = do
    f <- funBody <$> evalExpr fun
    evalParams <- forM params evalExpr
    evalFun f evalParams
    where
        funBody (Function fb) = fb
        funBody x = error $ "Not a function: " <> show x

evalFun :: FunDecl -> [Value] -> StateT State IO Value
evalFun (FunDecl _name args (FunBlock _decl body ret)) params = do
    -- create new stack frame
    runId newFrame

    -- insert params
    forM_ (zip args params) (\(a,p) -> runId (putsVar a p))

    -- run the body
    forM_ body evalStmt

    -- evaluate and return the result
    result <- evalExpr ret

    -- drop the frame before returning
    runId dropFrame

    pure result

-- -----------------

evalStmt :: Stmt -> StateT State IO ()

evalStmt (OutputStmt e) = do
    v <- evalExpr e
    lift $ print v
    pure ()

evalStmt this@(WhileStmt cond body) = do
    c <- evalExpr cond
    when (truthy c) $ do
        evalStmt body
        evalStmt this

evalStmt (IfStmt cond tru fals) = do
    c <- evalExpr cond
    if truthy c
        then evalStmt tru
        else traverse_ evalStmt fals

evalStmt (Block stmx) = do
    traverse_ evalStmt stmx

evalStmt (AssignmentStmt etarget eval) = do
    target <- evalExprForWrite etarget
    value <- evalExpr eval

    case target of
        Pointer addr -> do
            runId $ putsAddr addr value
        _ -> error $ "Cannot assign to read-only value " ++ show target


-- Eval expression which is target for writing. This works only for identifiers, record field access and *.
-- This returns a pointer, not the value.
evalExprForWrite :: Expr -> StateT State IO Value

evalExprForWrite (UnOp Deref e1) = do
    addr <- evalExpr e1
    case addr of 
        Pointer _ -> pure addr
        _ -> error $ "Deref encountered a non-pointer: " ++ show addr

evalExprForWrite (FieldAccess field id) = do
    fieldStruct <- evalExpr field
    case fieldStruct of
        Interpreter.Data.Record mapping ->
            maybe (error $ "Nonexistent field " ++ show id) (pure . Pointer) (lookup id mapping)
        _ -> error $ "Attempted to access field of " ++ show field

evalExprForWrite (EIdentifier id) =
    runId (getsVarAddr id) >>= maybe (error $ "Variable not found: " ++ id) (pure . Pointer)

evalExprForWrite e = error $ "Target to write is read-only: " ++ show e
