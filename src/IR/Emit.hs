module IR.Emit where
import IR.CompilerState
import Control.Monad.State
import IR.Tac
import Control.Monad.Writer
import Control.Monad

run :: (State CState a) -> TAC a
run = TAC . lift

emit :: (Reg -> Instr) -> TAC Reg
emit i = do
    resReg <- run reg
    l <- run label
    tell (IR [(l, i resReg)])
    pure resReg

emitL :: Instr -> TAC Label
emitL i = do
    l <- run label
    tell (IR [(l, i)])
    pure l

emit_ :: Instr -> TAC ()
emit_ instr = void (emitL instr)
