module IR.Desugar (desugar) where
import IR.Tac (ExtendedInstr(..), TinyCInstr(..), dreg, AnySource (..))
import IR.TacPrint ()
import Analysis.Typecheck.Type
import IR.CompilerState (Reg(..), Label)
import qualified Data.Map as M
import Parse.AST (Identifier)

desugar :: M.Map Identifier Label -> ExtendedInstr -> [TinyCInstr]
desugar _ (Native i) = [i]
desugar _ (Return r) = [Mov Bottom (dreg $ R 0) (dreg r), Ret]
desugar funmap (Call t target args) =
    [
        -- TODO: pass args correctly
        RCall (Imm (funmap M.! target))
    ]
desugar _ i = error $ "Unknown instruction to desugar: " <> show i
