module Compile.Compile (compile) where
import Parse.AST (Program)
import Text.Parsec (SourcePos)
import qualified IR.TacCompiler as IRCompiler
import Utils ((<$$>))
import IR.Desugar (desugar)
import Analysis.Typecheck.Type (Type)
import qualified Data.Map as M
import IR.CompilerState
import IR.Tac (TAC (TAC), TinyCInstr, fixupJumps, concatTAC)
import qualified Compile.Optimization.AST.Optimize as AST
import qualified Compile.Optimization.IR.Optimize as IR

compile :: Program (SourcePos, Type) -> Either String String
compile unOptProg = do
    -- AST-level optimization
    let prog = AST.optimize unOptProg

    -- Generate IR
    let (richIR, funInfo) = IRCompiler.compile (snd <$$> prog)

    -- TODO: optimize on IR

    -- Desugar IR to instructions supported by the underlying machine
    let rawIR = relabel $ concatTAC $ desugar (M.fromList funInfo) <$> richIR

    -- TODO: optimize on IR
    let optimizedIR = relabel $ IR.optimize rawIR

    -- TODO: register allocation

    -- Emit ASM
    Right $ ".text\n" <> show optimizedIR <> "\n"

-- Re-do labels and relocate jumps / functions to match that.
-- End result has every instruction annotated with a label, and they are consecutive.
relabel :: TAC TinyCInstr -> TAC TinyCInstr
relabel (TAC li) =
    let
        (relabelled, mapping) = relabel' li 0
        fixed = applySnd (fixupJumps (M.fromList mapping)) <$> relabelled
    in
        TAC fixed
    where
        -- change labels to be consecutive and return mapping needed to fix calls
        relabel' :: [(Maybe Label, a)] -> Label -> ([(Maybe Label, a)], [(Label, Label)])
        relabel' [] _ = ([], [])
        relabel' ((Just l, a):xs) next = (
                (Just next, a) : tailXs,
                (l, next) : tailMapping
            )
            where
                (tailXs, tailMapping) = relabel' xs (next + 1)
        relabel' ((Nothing, a):xs) next = (
                (Just next, a) : tailXs,
                tailMapping
            )
            where
                (tailXs, tailMapping) = relabel' xs (next + 1)


        applySnd :: (a -> b) -> (Maybe Label, a) -> (Maybe Label, b)
        applySnd f (ml, a) = (ml, f a)
