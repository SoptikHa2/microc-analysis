{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Analysis.Dataflow.Const where
import Analysis.Cfg.Cfg (CFGMap, CFGId, CFGNode(..), next, getId)
import qualified Data.Map as M
import Parse.AST
import Analysis.Dataflow.Lattices (ConstLattice)
import Control.Monad.State
import Lattice (Lattice(..), (<&&>), (<||>))
import Data.Maybe
import Data.Foldable

type ResultMap = M.Map CFGId ResultLat
type ResultLat = M.Map Identifier ConstLattice

-- TODO: generalize

-- Args:
--  id of previously changed CFG node (siblings will be rerun)
--  CFG map of the function
-- State:
--  Mapping from CFG Node -> (Variable -> Const value)
-- Returns:
--  List of affected CFG nodes
runStep :: CFGId -> CFGMap a -> State ResultMap [Int]
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

runCfg (Node id _ prevs stmt) = do
    -- 1) get assignments of all previous ones
    -- 2) use the statement to compute new constraints
    -- 3) replace self with new ones

    -- assumption: there is at least one child with the items
    prevAssignments <- catMaybes <$> gets (\m -> (m M.!?) <$> prevs)
    let mergedPrev = foldr1 (<||>) prevAssignments

    -- use the assumptions to compute self values
    let mine = computeStmt stmt mergedPrev

    existingSolution <- gets (M.!? id)

    let changed = case existingSolution of
                Just e | e == mine -> False
                _ -> True
    
    modify (M.insert id mine)
    
    pure changed

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
computeStmt (AssignmentStmt _ (EIdentifier _ variable) rhs) lat = undefined
computeStmt _ lat = lat

computeExpr :: Expr a -> ResultLat -> ConstLattice
computeExpr _ _ = undefined