module IR.Desugar (desugar) where
import IR.Tac (ExtendedInstr(..), TinyCInstr(..))
import IR.TacPrint ()

desugar :: ExtendedInstr -> TinyCInstr
desugar (Native i) = i
desugar i = error $ "Unknown instruction to desugar: " <> show i
