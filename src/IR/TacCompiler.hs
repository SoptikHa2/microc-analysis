{-# LANGUAGE RecursiveDo #-}
module IR.TacCompiler where
import IR.Tac
import IR.CompilerState
import Control.Monad.Writer
import IR.Emit
import Control.Monad.State
import Parse.AST
import qualified Data.Map as M
import Analysis.Typecheck.Type (Type(..))
import Data.Foldable (traverse_)


compile :: Program a -> TAC
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

-- TODO: this is weird
emitStmt (AssignmentStmt _ lhs rhs) = do
    target <- emitExpr lhs
    rval <- emitExpr rhs
    -- TODO: type. Also, based on the type,
    -- decide between Mov and MovIntoPtr?
    emit_ $ Mov Bottom target rval

emitExpr :: Expr a -> Emitter Reg
emitExpr = undefined

emitIf :: Emitter ()
emitIf = mdo
    compResult <- emit (Imm Int 0)

    emit_ $ Jz compResult fals
    emit_ $ Jmp tru

    tru <- emitL Nop
    emit_ $ Jmp end
    fals <- emitL Halt

    end <- emitL Nop

    pure ()
