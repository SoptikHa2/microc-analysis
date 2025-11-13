module IR.Emit where
import IR.CompilerState
import Control.Monad.State
import IR.Tac
import Control.Monad.Writer
import Control.Monad

run :: (State CState a) -> Emitter a
run = Emitter . lift

emit :: (Reg -> Instr) -> Emitter Reg
emit i = do
    resReg <- run reg
    l <- run label
    tell (TAC [(l, i resReg)])
    pure resReg

emitL :: Instr -> Emitter Label
emitL i = do
    l <- run label
    tell (TAC [(l, i)])
    pure l

emit_ :: Instr -> Emitter ()
emit_ instr = void (emitL instr)

runEmitter :: Emitter a -> (a, TAC)
runEmitter (Emitter m) =
  let ((a, ir), _) = runState (runWriterT m) empty
  in (a, ir)
