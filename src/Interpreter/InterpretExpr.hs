module Interpreter.InterpretExpr where
import Parse.AST
import Control.Monad.State (StateT)
import Interpreter.State
import Interpreter.Data

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
        Pointer addr -> undefined
        _ -> error "Attempted to deref non-pointer"

evalExpr (UnOp Ref (EIdentifier id)) = undefined
evalExpr (UnOp Ref _) = error "Attempted to take address of non-variable"

evalExpr (UnOp Alloc e1) = undefined

evalExpr Input = undefined

evalExpr Null = pure $ Pointer 0

