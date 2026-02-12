module IR.Desugar (desugar) where
import IR.Tac (ExtendedInstr(..), TinyCInstr(..), dreg, AnySource (..), AnyTarget (..))
import IR.TacPrint ()
import Analysis.Typecheck.Type
import IR.CompilerState (Reg(..), Label)
import qualified Data.Map as M
import Parse.AST (Identifier)

desugar :: M.Map Identifier Label -> ExtendedInstr -> [TinyCInstr]
desugar _ (Native i) = [i]
desugar _ (Return r) = [Mov Bottom (dreg $ R 0) (dreg r), Ret]
desugar funmap (Call _ target args) =
    (Push <$> args) <>
    [
        RCall (Imm (funmap M.! target))
    ]
desugar _ (RegCall _ reg args) =
    (Push <$> args) <>
    [
        RCall (Register reg)
    ]
desugar funmap (GetFunPtr t reg name) =
    [
        MovFunPtr t (dreg reg) (Direct0 $ Imm (funmap M.! name))
    ]
