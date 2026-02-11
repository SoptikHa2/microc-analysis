{-# LANGUAGE RecursiveDo #-}
module IR.Stdlib (
    funcs
) where
import IR.Tac
import Analysis.Typecheck.Type
import IR.Emit

funcs :: [Emitter ()]
funcs = [
    start,
    alloc
    ]

start :: Emitter ()
start = mdo
    -- Allocator: set that 0 is used (by the allocator)
    emit_ (Mov (Ptr Bottom) (Deref (Imm 0) (Imm 0)) (Direct0 $ Imm 0))

alloc :: Emitter ()
alloc = mdo
    obj <- emit (GetNthArg (Ptr Bottom) 0 . dreg)
    objsize <- emit (GetNthArg Int 1 . dreg)

    -- at addr 0, we store last used address
    nextAvailableAddr <- emit (\r -> Mov (Ptr Bottom) (dreg r) (Deref0 (Imm 0)))
    -- increment, and store
    emit_ (Add (Ptr Bottom) nextAvailableAddr (dreg objsize))
    emit_ (Mov (Ptr Bottom) (Deref0 (Imm 0)) (dreg nextAvailableAddr))
    -- copy object to the place
    -- TODO: loop if object is big
    emit_ (Mov Bottom (Deref0 (Register nextAvailableAddr)) (Deref0 (Register obj)))

    -- return the address
    emit_ $ Return nextAvailableAddr
