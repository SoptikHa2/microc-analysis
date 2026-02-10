module Compile.Optimization.IR.NopRemoval (optimize) where
import IR.Tac (TAC(..), TinyCInstr(Nop), fixupJumps)
import IR.CompilerState (Label)
import Data.Map (fromList)
import Data.Bifunctor (second)

optimize :: TAC TinyCInstr -> TAC TinyCInstr
-- Remove NOPs.
-- This removes every NOP, and if there are any jumps pointing at it, re-does its jump
-- so that they point to the next instruction instead.
-- This requires all NOPs and all consecutive instructions to have defined Label.
-- If some of them do not have their labels defined, they will be left as-is.
optimize (TAC prog) = TAC fixed
    where
        (rewritten, jumps) = rewriteAndGetChanges prog
        fixed = fixJumpsUntilFixedPoint jumps rewritten

        fixJumpsUntilFixedPoint :: [(Label, Label)] -> [(Maybe Label, TinyCInstr)] -> [(Maybe Label, TinyCInstr)]
        fixJumpsUntilFixedPoint jumps prog
            | prog == prog' = prog
            | otherwise     = fixJumpsUntilFixedPoint jumps prog'
            where
                prog' = second (fixupJumps (fromList jumps)) <$> prog

rewriteAndGetChanges :: [(Maybe Label, TinyCInstr)] -> ([(Maybe Label, TinyCInstr)], [(Label, Label)])
rewriteAndGetChanges [] = ([], [])
rewriteAndGetChanges ((Just ol, Nop):x@(Just nl, _):xs) =
    let
        (xs', rewrites) = rewriteAndGetChanges xs
    in
        (x : xs', (ol, nl): rewrites)
rewriteAndGetChanges (x:xs) =
    let
        (xs', rewrites) = rewriteAndGetChanges xs
    in
        (x : xs', rewrites)
