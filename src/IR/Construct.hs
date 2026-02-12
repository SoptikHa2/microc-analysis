module IR.Construct (constructRecord, constructArray) where
import Parse.AST (Expr, Record(..))
import IR.Tac
import IR.Emit
import IR.CompilerState (Reg)
import Analysis.Typecheck.Type (Type(..))
import Data.List (sortBy)

-- | Construct a record at `ptr`. Compiles field expressions via the given
--   emitter, then stores results at [ptr], [ptr-1], ... (sorted by field name).
constructRecord :: (Expr Type -> Emitter Reg) -> Reg -> Record Type -> Emitter ()
constructRecord emitRVal ptr (Fields fx) = do
    let exprs = snd <$> sortBy (\a b -> compare (fst a) (fst b)) fx
    regs <- traverse emitRVal exprs
    storeAt ptr regs

-- | Construct an array at `ptr`. Compiles element expressions via the given
--   emitter, then stores results at [ptr], [ptr-1], ...
constructArray :: (Expr Type -> Emitter Reg) -> Reg -> [Expr Type] -> Emitter ()
constructArray emitRVal ptr exprx = do
    regs <- traverse emitRVal exprx
    storeAt ptr regs

-- Store register values at [ptr], [ptr-1], [ptr-2], ...
storeAt :: Reg -> [Reg] -> Emitter ()
storeAt ptr regs =
    sequence_ [emit_ $ Mov Int (Deref (Register ptr) (Imm (negate i))) (dreg reg)
              | (i, reg) <- zip [0..] regs]
