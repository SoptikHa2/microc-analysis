module IR.CompilerState (CState(..), Reg(..), Label, empty, reg, label, getFun, saveFun, getVar, getVarMaybe) where

import Control.Monad.State
import qualified Data.Map as M
import Parse.AST (Identifier)
import Analysis.Typecheck.Type (Type, sizeof)
import Data.Maybe (fromJust)

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
    -- Each function saves its arguments into offset from BP. This remembers which 
    -- argument (and variable!) corresponds to which offset.
    funOffsetsForVars :: M.Map Identifier (M.Map Identifier Int)
}

empty :: CState
empty = CState { nextRegister = 1, nextLabel = 0, currentFunction = "", funBodies = M.empty, funOffsetsForVars = M.empty }

reg :: State CState Reg
reg = do
    reg <- gets nextRegister
    modify (\s -> s { nextRegister = reg + 1 })
    pure $ R reg

-- Get var offset from BP
getVarMaybe :: Identifier -> State CState (Maybe Int)
getVarMaybe var = do
    funName <- gets currentFunction
    regs <- gets funOffsetsForVars
    let regsFF = regs M.! funName
    pure $ regsFF M.!? var

-- Get var offset from BP
getVar :: Identifier -> State CState Int
getVar var = do
    mv <- getVarMaybe var
    pure $ fromJust mv

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

saveFun :: Identifier -> Label -> [(Identifier, Type)] -> [(Identifier, Type)] -> State CState ()
saveFun funName funLabel funVars argVars =
    modify 
        (\s@(CState _ _ _ funLabels oldFunRegs) ->
            s {
                currentFunction = funName,
                funBodies = M.insert funName funLabel funLabels,
                funOffsetsForVars = M.insert funName (M.fromList namesAndOffsets) oldFunRegs
            }
            )
    where
        varNames = fst <$> funVars
        varSizes = sizeof . snd <$> funVars
        varRunningSizes = scanl1 (+) varSizes
        varNamesAndOffsets = zip varNames (negate <$> varRunningSizes)

        argNames = fst <$> reverse argVars
        argSizes = sizeof . snd <$> reverse argVars
        argRunningSizes = scanl1 (+) argSizes
        argNamesAndOffsets = zip argNames ((+1) <$> argRunningSizes)

        namesAndOffsets = varNamesAndOffsets <> argNamesAndOffsets
