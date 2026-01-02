{-# LANGUAGE RecursiveDo #-}
module IR.Stdlib where
import IR.Tac
import Analysis.Typecheck.Type
import IR.Emit

funcs = [
    start,
    alloc
    ]

start :: Emitter ()
start = mdo
    -- Allocator: set that 0 is used (by the allocator)
    emit_ (Mov (Ptr Bottom) (Deref (Imm 0) 0) (Direct $ Imm 0))

alloc :: Emitter ()
alloc = mdo
    obj <- emit (GetNthArg (Ptr Bottom) 0 . dreg)
    objsize <- emit (GetNthArg Int 1 . dreg)

    -- at addr 0, we store last used address
    nextAvailableAddr <- emit (\r -> Mov (Ptr Bottom) (dreg r) (Deref (Imm 0) 0))
    -- increment, and store
    emit_ (Add (Ptr Bottom) nextAvailableAddr (dreg objsize))
    emit_ (Mov (Ptr Bottom) (Deref (Imm 0) 0) (dreg nextAvailableAddr))
    -- copy object to the place
    -- TODO: loop if object is big
    emit_ (Mov Bottom (Deref (Register nextAvailableAddr) 0) (Deref (Register obj) 0))

    -- return the address
    emit_ $ Ret nextAvailableAddr
