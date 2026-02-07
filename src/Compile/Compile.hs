module Compile.Compile (compile) where
import Parse.AST (Program)
import Text.Parsec (SourcePos)
import qualified IR.TacCompiler as IRCompiler
import Utils ((<$$>))
import IR.Desugar (desugar)
import Analysis.Typecheck.Type (Type)
import IR.Tac (concatTAC, TAC (..), TinyCInstr (..), AnySource (..))
import qualified Data.Map as M
import IR.CompilerState
import Data.Maybe (fromMaybe)

compile :: Program (SourcePos, Type) -> Either String String
compile prog = do
    -- TODO: optimize on AST

    -- Generate IR
    let (richIR, funInfo) = IRCompiler.compile (snd <$$> prog)

    -- TODO: optimize on IR

    -- Desugar IR to instructions supported by the underlying machine
    let rawIR = concatTAC $ desugar (M.fromList funInfo) <$> richIR

    -- TODO: optimize on IR

    -- TODO: register allocation

    -- Emit ASM
    Right $ ".text\n" <> show (relabel rawIR) <> "\n"

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

        fixupJumps :: M.Map Label Label -> TinyCInstr -> TinyCInstr
        fixupJumps m (RCall (Imm i)) = RCall (Imm (i `fromMaybe` (m M.!? i)))
        fixupJumps m (Jmp l) = Jmp (l `fromMaybe` (m M.!? l))
        fixupJumps m (Jz l) = Jz (l `fromMaybe` (m M.!? l))
        fixupJumps _ i = i

        applySnd :: (a -> b) -> (Maybe Label, a) -> (Maybe Label, b)
        applySnd f (ml, a) = (ml, f a)
