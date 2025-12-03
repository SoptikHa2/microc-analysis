{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Analysis.Dataflow.Const where
import Analysis.Cfg.Cfg (CFGMap, CFG(..), CFGId, CFGNode(..), next, getId)
import qualified Data.Map as M
import Parse.AST
import Analysis.Dataflow.Lattices (ConstLattice (..))
import Control.Monad.State
import Lattice (Lattice(..), (<&&>), (<||>))
import Data.Maybe
import Data.List
import Debug.Trace

type ResultMap = M.Map CFGId ResultLat
type ResultLat = M.Map Identifier ConstLattice

-- TODO: generalize

solve :: Show a => CFG a -> ResultMap
solve cfg@(CFG map root) = (trace ("With CFG:" ++ show map) (go map [root.id] initialResult))
    where
        -- Initialize the root node first
        (_, initialResult) = runState (runCfg root) M.empty

        go :: Show a => CFGMap a -> [CFGId] -> ResultMap -> ResultMap
        go map idxs result =
            if null newChanged
                then (trace ("Final result: " <> show newResultMap) newResultMap)
                else go map (nub newChanged) newResultMap
            where
                (newChanged, newResultMap) = foldr
                    (\id (changedIds,result) ->
                        let
                            (newChangedIds, newResult) = runState (runStep (trace ("Running for " ++ show id ++ " with " ++ show result) id) map) result
                        in
                            (newChangedIds <> changedIds, newResult)
                        )
                    ([],result)
                    idxs

emptyResultMap :: CFG a -> ResultMap
emptyResultMap (CFG map _root) = M.fromList (zip allKeys (repeat M.empty))
    where
        allKeys = M.keys map

-- Args:
--  id of previously changed CFG node (siblings will be rerun)
--  CFG map of the function
-- State:
--  Mapping from CFG Node -> (Variable -> Const value)
-- Returns:
--  List of affected CFG nodes
runStep :: Show a => CFGId -> CFGMap a -> State ResultMap [Int]
runStep toRun cfgMap = do
    -- First, retrieve the CFG
    let cfg = cfgMap M.! toRun
    -- This analysis will run for all children
    let children = next cfgMap cfg

    childrenThatChanged <- traverse runCfg children

    -- Return ids of nodes that changed
    let changedChildren = fst <$> filter snd (zip children childrenThatChanged)
    pure $ getId <$> changedChildren

-- Run a single CFG node. Update the 
-- node values in the map if anything changed, and return whether so
runCfg :: CFGNode a -> State ResultMap Bool
runCfg (FunEntry id _ vars _) = do
    -- The entry is always out-of-date only once
    m <- get
    case m M.!? id of
        Nothing -> do
            let defVars = zip vars (repeat bottom)
            let resLat = M.fromList defVars
            modify (M.insert id resLat)
            pure True
        Just _ -> pure False

runCfg (Node id prevs _ stmt) = do
    -- 1) get assignments of all previous ones
    -- 2) use the statement to compute new constraints
    -- 3) replace self with new ones

    prevAssignments <- catMaybes <$> gets (\m -> (m M.!?) <$> prevs)
    let mergedPrev = foldr (<||>) M.empty prevAssignments

    -- use the assumptions to compute self values
    let mine = computeStmt stmt mergedPrev

    existingSolution <- gets (M.!? id)

    let changed = case existingSolution of
                Just e | e == mine -> False
                _ -> True
    
    modify (M.insert id mine)
    
    pure (trace ("For id " ++ show id ++ " prev: " ++ show mergedPrev ++ " because " ++ show prevs ++ ", new: " ++ show mine) changed)

runCfg (FunExit id _ _ prevs) = do
    -- 1) get assignments of all previous ones
    -- 2) <|> them
    -- 3) insert into map, return if was equialent

    -- assumption: there is at least one child with the items
    prevAssignments <- catMaybes <$> gets (\m -> (m M.!?) <$> prevs)
    let mergedPrev = foldr1 (<||>) prevAssignments

    existingSolution <- gets (M.!? id)

    let changed = case existingSolution of
                Just e | e == mergedPrev -> False
                _ -> True
    
    modify (M.insert id mergedPrev)
    
    pure changed


computeStmt :: Stmt a -> ResultLat -> ResultLat
-- The only one that matters is Assignment. Block should not appear (this is CFG!)
computeStmt (Block _ _) _ = error "Block statement remained in CFG. This is illegal"
computeStmt (AssignmentStmt _ (EIdentifier _ variable) rhs) lat = M.insert variable cRhs lat
    where
        cRhs = computeExpr rhs lat
computeStmt _ lat = lat

computeExpr :: Expr a -> ResultLat -> ConstLattice
computeExpr (BiOp _ op l r) lat = runBiOp op lhs rhs
    where
        lhs = computeExpr l lat
        rhs = computeExpr r lat
computeExpr (UnOp _ op e) lat = runUnOp op expr
    where
        expr = computeExpr e lat
computeExpr (Input _) _ = top
computeExpr (Null _) _ = top
computeExpr (FieldAccess _ _ _) _ = top
computeExpr (ArrayAccess _ _ _) _ = top
computeExpr (Call _ _ _) _ = top
computeExpr (Record _ _) _ = bottom
computeExpr (Array _ _) _ = bottom
computeExpr (Number _ i) _ = Const i
computeExpr (EIdentifier _ id) lat = fromMaybe bottom (lat M.!? id)

runUnOp :: UnOp -> ConstLattice -> ConstLattice
runUnOp _ _ = bottom

runBiOp :: BiOp -> ConstLattice -> ConstLattice -> ConstLattice
runBiOp _ Bottom _ = Bottom
runBiOp _ _ Bottom = Bottom
runBiOp _ Top _ = Top
runBiOp _ _ Top = Top

runBiOp Eq (Const l) (Const r) = if l == r then Const 1 else Const 0
runBiOp Gt (Const l) (Const r) = if l > r then Const 1 else Const 0
runBiOp Plus (Const l) (Const r) = Const (l + r)
runBiOp Minus (Const l) (Const r) = Const (l - r)
runBiOp Mul (Const l) (Const r) = Const (l * r)
runBiOp Div (Const _) (Const r) | r == 0 = Bottom
runBiOp Div (Const l) (Const r) = Const (l `div` r)
