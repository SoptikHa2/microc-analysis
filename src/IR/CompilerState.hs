module IR.CompilerState (CState, Reg, Label, empty, reg, label) where

import Control.Monad.State
import qualified Data.Map as M
import Parse.AST (Identifier)

type Label = Int
type Reg = Int

data CState = CState {
    nextRegister :: Reg,
    nextLabel :: Label,
    -- Where each function begins.
    -- Impl detail: at first, this is Halt, and is replaced by jump to real body later when the function is actually compiled.
    funBodies :: M.Map Identifier Label
}

empty :: CState
empty = CState { nextRegister = 0, nextLabel = 0, funBodies = M.empty }

reg :: State CState Reg
reg = do
    reg <- gets nextRegister
    modify (\s -> s { nextRegister = reg + 1 })
    pure reg

label :: State CState Label
label = do
    label <- gets nextLabel
    modify (\s -> s { nextLabel = label + 1 })
    pure label

saveFun :: Identifier -> Label -> State CState ()
saveFun i l = modify (\s@(CState _ _ m) -> s { funBodies = M.insert i l m })

getFun :: Identifier -> State CState (Maybe Label)
getFun i = do
    funcs <- gets funBodies
    pure (M.lookup i funcs)
