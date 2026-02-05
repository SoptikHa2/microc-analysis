module IR.Emit where
import IR.CompilerState
import Control.Monad.State
import IR.Tac
import Control.Monad.Writer
import Control.Monad

class AnyTACEmit a where
    _constructEInstr :: a -> ExtendedInstr

    emit :: (Reg -> a) -> Emitter Reg
    emit i = do
        resReg <- run reg
        l <- run label
        tell (TAC [(Just l, _constructEInstr $ i resReg)])
        pure resReg

    emitL :: a -> Emitter Label
    emitL i = do
        l <- run label
        tell (TAC [(Just l, _constructEInstr i)])
        pure l
    
    emit_ :: a -> Emitter ()
    emit_ instr = void (emitL instr)

instance AnyTACEmit TinyCInstr where
  _constructEInstr = Native
instance AnyTACEmit ExtendedInstr where
  _constructEInstr = Prelude.id

run :: (State CState a) -> Emitter a
run = Emitter . lift

runEmitter :: Emitter a -> (a, ExtendedTAC)
runEmitter (Emitter m) =
  let ((a, ir), _) = runState (runWriterT m) empty
  in (a, ir)
