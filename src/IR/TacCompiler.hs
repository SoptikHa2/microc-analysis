{-# LANGUAGE RecursiveDo #-}
module IR.TacCompiler (compile) where
import IR.Tac
import IR.CompilerState
import IR.Emit
import Parse.AST
import Analysis.Typecheck.Type (Type(..), sizeof)
import Data.Foldable (traverse_, for_)
import Data.Traversable

entrypoint :: ExtendedTAC
entrypoint = TAC $
    (Nothing, IR.Tac.Call Int "main" []) :
    (Nothing, Native $ IR.Tac.PutNum (R 0)) :
    [(Nothing, Native IR.Tac.Halt)]

compile :: Program Type -> (ExtendedTAC, [(Identifier, Label)])
compile funcs = (entrypoint <> ir, funinfo)
    where
        funcsIR = traverse emitFun funcs
        (funinfo, ir) = runEmitter funcsIR

emitFun :: FunDecl Type -> Emitter (Identifier, Label)
emitFun f = do
    -- get args and vars
    let argTypes = case f.d of
            (Fun at _) -> at
            t -> error $ "Bad type for a function: " <> show t
    let varTypes = repeat Int -- TODO

    -- generate function entry and label it
    funLabel <- emitL Nop

    -- prolog:
    -- save previous BP
    emit_ $ Push (Register BP)
    -- new BP is at top of previous stack
    emit_ $ Mov (Ptr Int) (dreg BP) (dreg SP)
    -- make sure pushes don't overwrite variables and arguments (passed on-stack)
    -- note that we offset SP by only the LOCAL variables, since the args are on-stack passed :)
    -- TODO: the take can be removed when we remove the Repeat above
    let varsSize = sum $ sizeof <$> take (length f.body.idDecl) varTypes
    emit_ $ Sub (Ptr Int) SP (Direct $ Imm varsSize)

    run $ saveFun f.name funLabel (zip f.body.idDecl varTypes) (zip f.args argTypes)

    -- emit function body
    traverse_ emitStmt f.body.body

    -- emit the return into reg
    result <- emitExpr f.body.return

    -- epilog:
    -- restore stack
    emit_ $ Mov (Ptr Int) (dreg SP) (dreg BP)
    -- restore BP
    emit_ $ Pop (Ptr Int) BP

    emit_ $ Return result
    pure (f.name, funLabel)

emitStmt :: Stmt Type -> Emitter ()

emitStmt (OutputStmt _ e) = do
    r <- emitExpr e
    emit_ $ PutNum r

emitStmt (WhileStmt _ cond body) = mdo
    loopBegin <- emitL Nop
    jmp <- emitCondition cond

    emit_ $ jmp end
    -- body
    emitStmt body
    -- jmp to cond again
    emit_ $ Jmp loopBegin

    end <- emitL Nop
    pure ()

emitStmt (IfStmt _ cond tru fals) = mdo
    jmp <- emitCondition cond
    emit_ $ jmp falsLabel

    emitStmt tru
    emit_ $ Jmp end

    falsLabel <- case fals of
        Just fals -> do
            fStart <- emitL Nop
            emitStmt fals
            pure fStart
        Nothing -> emitL Nop

    end <- emitL Nop
    pure ()

emitStmt (Block _ stmx) = traverse_ emitStmt stmx

emitStmt (AssignmentStmt _ lhs rhs) = do
    let rhsType = exprData rhs
    target <- emitLAddrExpr lhs
    rval <- emitRValExpr rhs
    case rhsType of
        -- If we are storing function pointer, we need to make sure to mark it; as we are relocating functions later
        Fun _ _ -> emit_ $ MovFunPtr rhsType target (dreg rval)
        -- Fallback (ints and stuff)
        _ -> emit_ $ Mov rhsType target (dreg rval)

-- Emit an expression, but we don't care about the result - just about the flags.
-- Returns the jump that should be used to get to False branch (NOT successful (it's more convenient))
emitCondition :: Expr Type -> Emitter (Label -> TinyCInstr)
emitCondition (BiOp t Eq lhs rhs) = do
    _ <- emitGenericBinOp (const Cmp) t lhs rhs
    pure Jnz
emitCondition (BiOp t Gt lhs rhs) = do
    _ <- emitGenericBinOp (const Cmp) t lhs rhs
    pure Jle
-- If the expression is not == or >; compare it with equality to non-zero (or zero if prefixed with Not)
emitCondition (UnOp _ Parse.AST.Not expr) = do
    r <- emitExpr expr
    emit_ $ Cmp r (Direct $ Imm 0)
    pure Jnz
emitCondition expr = do
    r <- emitExpr expr
    emit_ $ Cmp r (Direct $ Imm 0)
    pure Jz

emitGenericBinOp :: (Type -> Reg -> AnyTarget -> TinyCInstr) -> Type -> Expr Type -> Expr Type -> Emitter Reg
emitGenericBinOp op t l r = do
    lreg <- emitExpr l
    rreg <- emitExpr r
    emit_ $ op t lreg (dreg rreg)
    pure lreg

emitExpr :: Expr Type -> Emitter Reg
emitExpr (BiOp _ Plus lhs rhs) = emitGenericBinOp Add Int lhs rhs
emitExpr (BiOp _ Minus lhs rhs) = emitGenericBinOp Sub Int lhs rhs
emitExpr (BiOp _ Parse.AST.Mul lhs rhs) = emitGenericBinOp IR.Tac.Mul Int lhs rhs
emitExpr (BiOp _ Parse.AST.Div lhs rhs) = emitGenericBinOp IR.Tac.Div Int lhs rhs

-- a == b: returns 1 if equal, 0 otherwise
emitExpr (BiOp t Eq lhs rhs) = mdo
    lreg <- emitExpr lhs
    rreg <- emitExpr rhs
    result <- emit (\r -> Mov t (dreg r) (Direct $ Imm 0))
    emit_ $ Cmp lreg (dreg rreg)
    emit_ $ Jnz end
    emit_ $ Mov t (dreg result) (Direct $ Imm 1)
    end <- emitL Nop
    pure result

-- a > b: returns 1 if greater, 0 otherwise
emitExpr (BiOp t Gt lhs rhs) = mdo
    lreg <- emitExpr lhs
    rreg <- emitExpr rhs
    result <- emit (\r -> Mov t (dreg r) (Direct $ Imm 0))
    emit_ $ Cmp lreg (dreg rreg)
    emit_ $ Jle end
    emit_ $ Mov t (dreg result) (Direct $ Imm 1)
    end <- emitL Nop
    pure result

emitExpr (UnOp t Parse.AST.Deref rhs) = do
    val <- emitExpr rhs
    -- todo: type
    -- mov target [rhs]
    emit (\r -> Mov t (dreg r) (IR.Tac.Deref (Register val) 0))

emitExpr (UnOp t Ref rhs) = do
    addr <- emitLAddrExpr rhs
    emit (\r -> Lea t r addr)

emitExpr (UnOp _ Alloc _) = undefined

-- !x: returns 1 if x == 0, 0 otherwise
emitExpr (UnOp t Parse.AST.Not rhs) = mdo
    val <- emitExpr rhs
    result <- emit (\r -> Mov t (dreg r) (Direct $ Imm 0))
    emit_ $ Cmp val (Direct $ Imm 0)
    emit_ $ Jnz end
    emit_ $ Mov t (dreg result) (Direct $ Imm 1)
    end <- emitL Nop
    pure result

emitExpr (Parse.AST.Call t (EIdentifier targetT target) args) = mdo
    ex <- traverse emitRValExpr args

    -- if the target is a variable, treat it as a function pointer instead
    funPtr <- run $ getVarMaybe target
    case funPtr of
        Just offset -> do
            varReg <- emit (\r -> Mov targetT (dreg r) (IR.Tac.Deref (Register BP) offset))
            emit_ $ IR.Tac.RegCall t varReg (Register <$> ex)
        Nothing -> emit_ $ IR.Tac.Call t target (Register <$> ex)

    -- remove pushed parameters to the function
    emit_ $ Sub (Ptr Int) SP (Direct $ Imm (length args))

    pure (R 0)

emitExpr (Number t i) = do
    emit (\r -> Mov t (Direct $ Register r) (Direct $ Imm i))

emitExpr expr@(EIdentifier t _) = do
    -- get the variable address
    var <- emitLAddrExpr expr
    -- save it to a new register
    emit (\r -> Mov t (dreg r) var)

emitExpr e = error $ "Compiling expr " <> show e <> " is not defined"

emitRValExpr :: Expr Type -> Emitter Reg
emitRValExpr (EIdentifier t@(Fun _ _) e) = do
    emit $ \r -> GetFunPtr t r e
emitRValExpr e = emitExpr e

-- Get address of the target expression to write into
emitLAddrExpr :: Expr Type -> Emitter AnyTarget
emitLAddrExpr (EIdentifier _ e) = do
    offset <- run $ getVar e
    pure $ IR.Tac.Deref (Register BP) offset
emitLAddrExpr (UnOp t Parse.AST.Deref nestedExpr) = do
    nestedAddr <- emitLAddrExpr nestedExpr
    nestedAddrReg <- emit (\r -> Mov t (dreg r) nestedAddr)
    pure $ IR.Tac.Deref (Register nestedAddrReg) 0
emitLAddrExpr e = error $ "I don't know how to get address of " <> show e <> " (for assignment)."
