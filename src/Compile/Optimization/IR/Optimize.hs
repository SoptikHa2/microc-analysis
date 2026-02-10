module Compile.Optimization.IR.Optimize (optimize) where
import IR.Tac (TinyCInstr, TAC)
import qualified Compile.Optimization.IR.NopRemoval as NopRemoval

optimize :: TAC TinyCInstr -> TAC TinyCInstr
optimize prog
    | prog == prog' = prog
    | otherwise     = optimize prog'
    where
        prog' = NopRemoval.optimize prog
