module IR.CompilerState (CState(..), Reg(..), Label, empty, reg, label, getFun, saveFun, getVarReg, getVarMaybe) where

import Control.Monad.State
import qualified Data.Map as M
import Parse.AST (Identifier)

type Label = Int
data Reg
    = BP
    | SP
    | R Int
    deriving (Eq)

data CState = CState {
    nextRegister :: Int,
    nextLabel :: Label,
    currentFunction :: Identifier,
    -- Where each function begins.
    funBodies :: M.Map Identifier Label,
    -- Each function saves its arguments into a set of registers. This remembers which 
    -- argument (and variable!) corresponds to which register
    funRegsForVars :: M.Map Identifier (M.Map Identifier Reg)
}

empty :: CState
empty = CState { nextRegister = 1, nextLabel = 0, funBodies = M.empty, funRegsForVars = M.empty }

reg :: State CState Reg
reg = do
    reg <- gets nextRegister
    modify (\s -> s { nextRegister = reg + 1 })
    pure $ R reg

-- Get a register corresponding to a variable, if there is one already
getVarMaybe :: Identifier -> State CState (Maybe Reg)
getVarMaybe var = do
    funName <- gets currentFunction
    regs <- gets funRegsForVars
    let regsFF = regs M.! funName
    pure $ regsFF M.!? var

setVar :: Identifier -> Reg -> State CState ()
setVar var reg = do
    funName <- gets currentFunction
    regs <- gets funRegsForVars
    let regsFF = regs M.! funName
    let newRegsFF = M.insert var reg regsFF
    let newRegs = M.insert funName newRegsFF regs
    modify (\s -> s { funRegsForVars = newRegs }) 

-- Get a register corresponding to a variable, or create one
getVarReg :: Identifier -> State CState Reg
getVarReg var = do
    maybeReg <- getVarMaybe var
    case maybeReg of
        Just reg -> pure reg
        Nothing -> do
            r <- reg
            setVar var r
            pure r

label :: State CState Label
label = do
    label <- gets nextLabel
    modify (\s -> s { nextLabel = label + 1 })
    pure label

--saveFun :: Identifier -> Label -> State CState ()
--saveFun i l = modify (\s@(CState _ _ m _) -> s { funBodies = M.insert i l m })

getFun :: Identifier -> State CState (Maybe Label)
getFun i = do
    funcs <- gets funBodies
    pure (M.lookup i funcs)

saveFun :: Identifier -> Label -> [(Identifier, Reg)] -> State CState ()
saveFun funName funLabel funRegs =
    modify 
        (\s@(CState _ _ _ funLabels oldFunRegs) ->
            s {
                currentFunction = funName,
                funBodies = M.insert funName funLabel funLabels,
                funRegsForVars = M.insert funName (M.fromList funRegs) oldFunRegs
            }
            )
