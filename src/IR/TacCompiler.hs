{-# LANGUAGE RecursiveDo #-}
module IR.TacCompiler (compile) where
import IR.Tac
import IR.CompilerState
import IR.Emit
import Parse.AST
import Analysis.Typecheck.Type (Type(..))
import Data.Foldable (traverse_)

entrypoint :: ExtendedTAC
entrypoint = TAC $
    (Nothing, IR.Tac.Call Int "main" [] (dreg $ R 0)) :
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
    let argVars = f.args <> f.body.idDecl
    -- generate registers for them
    varRegs <- traverse (const $ run reg) argVars
    -- generate nop and label it
    funLabel <- emitL Nop

    run $ saveFun f.name funLabel (zip argVars varRegs)

    -- emit function body
    traverse_ emitStmt f.body.body

    -- emit the return into reg
    result <- emitExpr f.body.return

    emit_ $ Return result
    pure (f.name, funLabel)

emitStmt :: Stmt Type -> Emitter ()

emitStmt (OutputStmt _ e) = do
    r <- emitExpr e
    emit_ $ Output r

emitStmt (WhileStmt _ cond body) = mdo
    loopBegin <- emitL Nop
    condResult <- emitExpr cond

    emit_ $ Jz condResult end
    -- body
    emitStmt body
    -- jmp to cond again
    emit_ $ Jmp loopBegin

    end <- emitL Nop
    pure ()

emitStmt (IfStmt _ cond tru fals) = mdo
    condResult <- emitExpr cond

    emit_ $ Jz condResult falsLabel

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

emitStmt (AssignmentStmt t lhs rhs) = do
    target <- emitExpr lhs
    rval <- emitExpr rhs
    -- TODO: Based on the type,
    -- decide between Mov and MovIntoPtr?
    emit_ $ Mov t (dreg target) (dreg rval)

emitGenericBinOp :: (Type -> Reg -> AnyTarget -> TinyCInstr) -> Type -> Expr Type -> Expr Type -> Emitter Reg
emitGenericBinOp op t l r = do
    lreg <- emitExpr l
    rreg <- emitExpr r
    emit_ $ op t lreg (dreg rreg)
    pure lreg

emitExpr :: Expr Type -> Emitter Reg
-- todo: types
emitExpr (BiOp _ Plus lhs rhs) = emitGenericBinOp Add Int lhs rhs
emitExpr (BiOp _ Minus lhs rhs) = emitGenericBinOp Sub Int lhs rhs
emitExpr (BiOp _ Parse.AST.Mul lhs rhs) = emitGenericBinOp IR.Tac.Mul Int lhs rhs
emitExpr (BiOp _ Parse.AST.Div lhs rhs) = emitGenericBinOp IR.Tac.Div Int lhs rhs
-- TODO: eq, gt

emitExpr (UnOp t Parse.AST.Deref rhs) = do
    val <- emitExpr rhs
    -- todo: type
    -- mov target [rhs]
    emit (\r -> Mov t (dreg r) (IR.Tac.Deref (Register val) 0))

emitExpr (UnOp t Ref rhs) = do
    val <- emitExpr rhs
    -- todo: type
    -- lea
    -- todo: verify
    emit (\r -> Lea t r (Register val))

emitExpr (UnOp t Alloc rhs) = undefined

emitExpr (UnOp t Parse.AST.Not rhs) = undefined

emitExpr (Number t i) = do
    emit (\r -> Mov t (Direct $ Register r) (Direct $ Imm i))

emitExpr (EIdentifier _ e) = do
    run $ getVarReg e

emitExpr e = error $ "Compiling expr " <> show e <> " is not defined"
