{-# LANGUAGE RecursiveDo #-}
module IR.TacCompiler (compile) where
import IR.Tac
import IR.CompilerState
import IR.Emit
import Parse.AST
import Analysis.Typecheck.Type (Type(..))
import Data.Foldable (traverse_)


compile :: Program a -> ExtendedTAC
compile funcs = ir
    where
        funcsIR = traverse emitFun funcs
        (_, ir) = runEmitter funcsIR

emitFun :: FunDecl a -> Emitter ()
emitFun f = do
    -- get args and vars
    let argVars = f.args <> f.body.idDecl
    -- generate registers for them
    varRegs <- traverse (const $ run reg) argVars
    -- generate nop and label it (TODO: label first el of body instead)
    funLabel <- emitL Nop

    run $ saveFun f.name funLabel (zip argVars varRegs)

    -- emit function body
    traverse_ emitStmt f.body.body

    -- emit the return into reg
    result <- emitExpr f.body.return

    emit_ $ Ret result

emitStmt :: Stmt a -> Emitter ()

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

emitStmt (AssignmentStmt _ lhs rhs) = do
    target <- emitExpr lhs
    rval <- emitExpr rhs
    -- TODO: type. Also, based on the type,
    -- decide between Mov and MovIntoPtr?
    emit_ $ Mov Bottom (dreg target) (dreg rval)

emitExpr :: Expr a -> Emitter Reg
-- todo: binop generic handling
emitExpr (BiOp _ Plus lhs rhs) = do
    lreg <- emitExpr lhs
    rreg <- emitExpr rhs
    emit_ $ Add Int lreg (dreg rreg)
    pure lreg 

emitExpr (UnOp _ Parse.AST.Deref rhs) = do
    val <- emitExpr rhs
    -- todo: type
    -- mov target [rhs]
    emit (\r -> Mov Bottom (dreg r) (IR.Tac.Deref (Register val)))

emitExpr (UnOp _ Ref rhs) = do
    val <- emitExpr rhs
    -- todo: type
    -- lea
    -- todo: verify
    emit (\r -> Lea Bottom r (Register val))

emitExpr (UnOp _ Alloc rhs) = do
    val <- emitExpr rhs
    -- todo: type
    -- todo: resolve label of $alloc
    -- todo: pass object size
    -- call $alloc rhs
    emit (IR.Tac.Call Bottom (-1) [dreg val] . dreg)

emitExpr (Number _ i) = do
    emit (\r -> Mov Int (Direct $ Register r) (Direct $ Imm i))

emitExpr (EIdentifier _ e) = do
    run $ getVarReg e

emitExpr e = error $ "Compiling expr " <> show e <> " is not defined"
