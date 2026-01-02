module IR.Desugar (desugar) where
import IR.Tac (ExtendedInstr(..), TinyCInstr(..), dreg)
import IR.TacPrint ()
import Analysis.Typecheck.Type
import IR.CompilerState (Reg(..))

desugar :: ExtendedInstr -> TinyCInstr
desugar (Native i) = i
desugar (Ret r) = Mov Bottom (dreg $ R 0) (dreg r)
desugar i = error $ "Unknown instruction to desugar: " <> show i
