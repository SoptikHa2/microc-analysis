module IR.Alloc (
    allocInit,
    alloc,
    alloc1
) where
import IR.Tac
import Analysis.Typecheck.Type
import IR.Emit
import IR.CompilerState

-- Allocator:
-- at index 0, there is address of the next free address
-- we never free for simplicity

-- Initialize allocator
allocInit :: Emitter ()
allocInit = emit_ $ Mov Int (Deref0 $ Imm 0) (Direct0 $ Imm 1)

-- Allocate memory, return the address
alloc :: Int -> Emitter Reg
alloc requiredBytes = do
    addrToAssign <- emit (\r -> Mov Int (dreg r) (Deref0 $ Imm 0))
    -- add the offset
    emit_ $ Add Int addrToAssign (Direct0 $ Imm requiredBytes)
    -- save the value + 1 into next free address
    emit_ $ Mov Int (Deref0 $ Imm 0) (Direct (Register addrToAssign) (Imm 1)) 
    pure addrToAssign

-- Allocate one memory cell and get the address
alloc1 :: Emitter Reg
alloc1 = do
    addrToRet <- emit (\r -> Mov Int (dreg r) (Deref0 $ Imm 0))
    emit_ $ Mov Int (Deref0 $ Imm 0) (Direct (Register addrToRet) (Imm 1))
    pure addrToRet
