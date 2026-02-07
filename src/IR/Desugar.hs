module IR.Desugar (desugar) where
import IR.Tac (ExtendedInstr(..), TinyCInstr(..), dreg, AnySource (..))
import IR.TacPrint ()
import Analysis.Typecheck.Type
import IR.CompilerState (Reg(..))

desugar :: ExtendedInstr -> [TinyCInstr]
desugar (Native i) = [i]
desugar (Return r) = [Mov Bottom (dreg $ R 0) (dreg r), Ret]
desugar (Call t target args ret) = -- TODO: find the correct label
    [
        -- TODO: pass args correctly
        RCall (Imm (-1)),
        Mov t (dreg $ R 0) ret
        
    ]
desugar i = error $ "Unknown instruction to desugar: " <> show i
