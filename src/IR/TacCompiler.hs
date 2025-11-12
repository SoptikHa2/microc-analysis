{-# LANGUAGE RecursiveDo #-}
module IR.TacCompiler where
import IR.Tac
import IR.CompilerState
import Control.Monad.Writer
import IR.Emit
import Control.Monad.State
import Parse.AST


compile :: Program a -> IR
compile funcs = ir
    where
        funcsIR = traverse emitFun funcs
        (_, ir) = runTAC funcsIR

-- emitFun :: FunDecl a -> TAC ()
-- emitFun f = do
--     f.

emitIf :: TAC ()
emitIf = mdo
    compResult <- emit (Imm 0)

    emit_ $ Jz compResult fals
    emit_ $ Jmp tru

    tru <- emitL Ret
    emit_ $ Jmp end
    fals <- emitL Halt

    end <- emitL Nop

    pure ()


runTAC :: TAC a -> (a, IR)
runTAC (TAC m) =
  let ((a, ir), _) = runState (runWriterT m) empty
  in (a, ir)
