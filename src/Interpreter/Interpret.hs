module Interpreter.Interpret (evalExpr, applyBiOp, evalStmt, evalExprForWrite, evalFun) where

import Prelude hiding (id)
import Parse.AST hiding (target, body, args, name)
import Interpreter.State
import Interpreter.Data
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.State (StateT, mapStateT, MonadTrans (lift), MonadIO (liftIO), execStateT, evalStateT)
import Control.Monad (forM, forM_, when)
import Data.Foldable (traverse_)
import Text.Parsec (SourcePos)
import Error (MicroCError(EInterpreter))
import Control.Exception (throw)
import Utils
import qualified Interpreter.State as IS
import System.Exit (exitFailure)

runId :: StateT State Identity a -> StateT State IO a
runId = mapStateT (pure . runIdentity)

errwlIO :: SourcePos -> String -> IO a
errwlIO loc text = throw $ EInterpreter $ "At " <> show loc <> ": " <> text

errwl :: SourcePos -> String -> StateT State IO a
errwl loc text = liftIO $ errwlIO loc text

applyBiOp :: SourcePos -> BiOp -> Value -> Value -> IO Value
applyBiOp _ Eq (VNumber v1) (VNumber v2) = pure $ VNumber $ if v1 == v2 then 1 else 0
applyBiOp _ Eq (Pointer p1) (Pointer p2) = pure $ VNumber $ if p1 == p2 then 1 else 0
applyBiOp _ Gt (VNumber i1) (VNumber i2) = pure $ VNumber $ if i1 > i2 then 1 else 0
applyBiOp _ Plus (VNumber i1) (VNumber i2) = pure $ VNumber $ i1 + i2
applyBiOp _ Minus (VNumber i1) (VNumber i2) = pure $ VNumber $ i1 - i2
applyBiOp _ Mul (VNumber i1) (VNumber i2) = pure $ VNumber $ i1 * i2
applyBiOp loc Div (VNumber i1) (VNumber i2) = if i2 == 0
    then errwlIO loc "Division by zero"
    else pure $ VNumber $ i1 `div` i2
applyBiOp loc op v1 v2 = errwlIO loc $ "Undefined operation " ++ show op ++ " on " ++ show v1 ++ ", " ++ show v2

evalExpr :: Expr SourcePos -> StateT State IO Value
evalExpr (BiOp loc op e1 e2) = do
    ee1 <- evalExpr e1
    ee2 <- evalExpr e2
    liftIO $ applyBiOp loc op ee1 ee2

evalExpr (UnOp loc Deref e1) = do
    ee1 <- evalExpr e1
    case ee1 of
        Pointer addr -> do
            v <- runId $ getsAddr addr
            case v of
                Just value -> pure value
                Nothing -> errwl loc $ "Dangling pointer: " ++ show addr
        _ -> errwl loc "Attempted to deref non-pointer"

evalExpr (UnOp loc Ref (EIdentifier _ id)) = do
    v <- runId $ getsVarAddr id
    case v of
        Just value -> pure $ Pointer value
        Nothing -> errwl loc $ "Undefined variable " ++ show id

evalExpr (UnOp loc Ref (FieldAccess _ record field)) = do
    fieldStruct <- evalExpr record
    case fieldStruct of
        Interpreter.Data.Record mapping -> do
            case lookup field mapping of
                Just addr -> pure $ Pointer addr
                Nothing -> errwl loc $ "Nonexistent field " ++ show field
        _ -> errwl loc $ "Attempted to access field of " ++ show record

evalExpr (UnOp loc Ref (ArrayAccess _ targetE idxE)) = do
    target <- evalExpr targetE
    idx <- evalExpr idxE
    case (target, idx) of
        (ArrayRef addrx, VNumber i) -> do
            case addrx !? i of
                Just addr -> pure $ Pointer addr
                Nothing -> errwl loc $ "Bad index " ++ show i
        _ -> errwl loc $ "Bad array subscript: " ++ show target ++ "[" ++ show idx ++ "]"

evalExpr (UnOp loc Ref _) = errwl loc "Attempted to take address of non-variable"

evalExpr (UnOp _ Alloc e1) = do
    valueToStore <- evalExpr e1
    addr <- runId $ putsValue valueToStore
    pure $ Pointer addr

evalExpr (UnOp _ Not e1) = do
    VNumber v <- evalExpr e1
    pure $ VNumber (if v == 0 then 1 else 0)

evalExpr (Input _) = do
    -- Maybe more datatypes?
    num <- lift (readLn :: IO Int)
    pure $ VNumber num

evalExpr (Null _) = pure $ Pointer 0

evalExpr fa@(FieldAccess loc _ id) = do
    Pointer ptr <- evalExpr (UnOp loc Ref fa)
    runId (getsAddr ptr) >>= maybe (errwl loc $ "Invalid field " ++ show id) pure

evalExpr aa@(ArrayAccess loc _ id) = do
    Pointer ptr <- evalExpr (UnOp loc Ref aa)
    runId (getsAddr ptr) >>= maybe (errwl loc $ "Invalid array item " ++ show id) pure

evalExpr (Parse.AST.Record _ (Fields fx)) = do
    -- Evaluate all the fields
    rr <- forM fx (\(n, value) -> (,) n <$> evalExpr value)
    -- Save them into memory
    ar <- forM rr (\(n, value) -> (,) n <$> runId (putsValue value))
    pure $ Interpreter.Data.Record ar

evalExpr (Parse.AST.Array _ items) = do
    -- Evaluate all items
    ix <- traverse evalExpr items
    -- Save into memory
    addr <- runId (traverse putsValue ix)
    pure $ ArrayRef addr

evalExpr (Number _ i) = pure $ VNumber i

evalExpr (EIdentifier loc id) = do
    val <- runId $ getsVar id
    case val of
        Just v -> pure v
        Nothing -> errwl loc $ "Unknown variable " ++ id

evalExpr (Call loc fun params) = do
    target <- evalExpr fun
    f <- funBody target
    evalParams <- forM params evalExpr
    evalFun f loc evalParams
    where
        funBody (Function fb) = pure fb
        funBody x = errwl loc $ "Not a function: " <> show x

evalFun :: FunDecl SourcePos -> SourcePos -> [Value] -> StateT State IO Value
evalFun (FunDecl _ name args (FunBlock _ decl body ret)) callSite params = do
    when (length args /= length params) (errwl callSite $
        "Expected " <> show (length args) <> " params for function " <> name)

    -- create new stack frame
    runId newFrame

    -- insert params
    forM_ (zip args params) (\(a,p) -> runId (putsVar a p))

    -- initialize declared variables to 0
    forM_ decl (\varName -> runId (putsVar varName (VNumber 0)))

    -- run the body
    forM_ body evalStmt

    -- evaluate and return the result
    result <- evalExpr ret

    -- drop the frame before returning
    runId dropFrame

    pure result

-- -----------------

evalStmt :: Stmt SourcePos -> StateT State IO ()

evalStmt (OutputStmt _ e) = do
    v <- evalExpr e
    lift $ print v
    pure ()

evalStmt this@(WhileStmt _ cond body) = do
    c <- evalExpr cond
    when (truthy c) $ do
        evalStmt body
        evalStmt this

evalStmt (IfStmt _ cond tru fals) = do
    c <- evalExpr cond
    if truthy c
        then evalStmt tru
        else traverse_ evalStmt fals

evalStmt (Block _ stmx) = do
    traverse_ evalStmt stmx

evalStmt (AssignmentStmt loc etarget eval) = do
    target <- evalExprForWrite etarget
    value <- evalExpr eval >>= copyVal
    case target of
        Pointer addr -> do
            runId $ putsAddr addr value
        _ -> errwl loc $ "Cannot assign to read-only value " ++ show target


-- Eval expression which is target for writing. This works only for identifiers, record field access and *.
-- This returns a pointer, not the value.
evalExprForWrite :: Expr SourcePos -> StateT State IO Value

evalExprForWrite (UnOp loc Deref e1) = do
    addr <- evalExpr e1
    case addr of
        Pointer nil | nil == 0 -> errwl loc "Null-pointer dereference"
        Pointer _ -> pure addr
        _ -> errwl loc $ "Deref encountered a non-pointer: " ++ show addr

evalExprForWrite (FieldAccess loc field id) = do
    fieldStruct <- evalExpr field
    case fieldStruct of
        Interpreter.Data.Record mapping ->
            maybe (errwl loc $ "Nonexistent field " ++ show id) (pure . Pointer) (lookup id mapping)
        _ -> errwl loc $ "Attempted to access field of " ++ show field

evalExprForWrite aa@(ArrayAccess loc _ _) = evalExpr (UnOp loc Ref aa)

evalExprForWrite (EIdentifier loc id) =
    runId (getsVarAddr id) >>= maybe (errwl loc $ "Variable not found: " ++ id) (pure . Pointer)

evalExprForWrite e = throw $ EInterpreter $ "Target to write is read-only: " ++ show e


copyVal :: Value -> StateT State IO Value
-- We need to deep-copy records
copyVal (Interpreter.Data.Record fields) = do
    -- For each field, we need to 1) deref the address
    -- 2) allocate the result on the heap
    newPtrs <- traverse (\(_, a) -> runId (copyPtr a)) fields
    pure $ Interpreter.Data.Record $ zip (fst <$> fields) newPtrs
    where
        copyPtr :: Address -> StateT State Identity Address
        copyPtr addr = do
            val <- getsAddr addr
            result <- traverse putsValue val
            case result of
                Just a -> pure a
                Nothing -> throw $ EInterpreter $ "Record contained bad pointer " <> show addr
-- For anything else, we don't care
copyVal x = pure x