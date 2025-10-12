module Interpreter.InterpretExpr (evalExpr) where

import Prelude hiding (id)
import Parse.AST
import Interpreter.State
import Interpreter.Data
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.State (StateT, mapStateT, MonadTrans (lift))

runId :: StateT State Identity a -> StateT State IO a
runId = mapStateT (pure . runIdentity)

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
    v <- runId $ getsVar id
    case v of
        Just value -> pure value
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
        Interpreter.Data.Record mapping -> 
            case lookup id mapping of
                Just addr -> do
                    val <- runId $ getsAddr addr
                    case val of
                        Just v -> pure v
                        Nothing -> error $ "Invalid field " ++ id ++ " of a struct"
                Nothing -> error $ "Attempted to access nonexistent field " ++ show id
        _ -> error $ "Attempted to access field of " ++ show e

evalExpr _ = undefined