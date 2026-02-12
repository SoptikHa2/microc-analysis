module IR.Construct (constructRecord, constructArray, deepCopyRecord) where
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
    storeAt (Register ptr) (dreg <$> regs)

-- | Construct an array at `ptr`. Compiles element expressions via the given
--   emitter, then stores results at [ptr], [ptr-1], ...
constructArray :: (Expr Type -> Emitter Reg) -> Reg -> [Expr Type] -> Emitter ()
constructArray emitRVal ptr exprx = do
    regs <- traverse emitRVal exprx
    storeAt (Register ptr) (dreg <$> regs)

-- Store register values at [ptr], [ptr-1], [ptr-2], ...
storeAt :: AnySource -> [AnyTarget] -> Emitter ()
storeAt ptr targets =
    sequence_ [emit_ $ Mov Int (Deref ptr (Imm (negate i))) target
              | (i, target) <- zip [0..] targets]

-- Copy record fields from src pointer to dst pointer.
-- Fields are at [ptr], [ptr-1], [ptr-2], ... (negative offsets).
deepCopyRecord :: Type -> AnySource -> AnySource -> Emitter ()
deepCopyRecord (Record rx) src dst = do
    let n = length rx
    let sources = [Deref src (Imm (negate i)) | i <- [0..n-1]]
    storeAt dst sources
deepCopyRecord t _ _ = error $ "Don't know how to deep copy " <> show t
