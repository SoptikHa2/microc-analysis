{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Analysis.Dataflow.Const where
import Analysis.Cfg.Cfg (CFGMap, CFGId, CFGNode(..), next, getId)
import qualified Data.Map as M
import Parse.AST
import Analysis.Dataflow.Lattices (ConstLattice)
import Control.Monad.State
import Lattice (Lattice(bottom))

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
    m <- get
    case m M.!? id of
        Nothing -> do
            let defVars = zip vars (repeat bottom)
            let resLat = M.fromList defVars
            modify (M.insert id resLat)
            pure True
        Just _ -> pure False


runCfg (FunExit id _ _ prevs) = do
    -- 1) get assignments of all previous ones
    -- 2) <&> them
    -- 3) insert into map, return if was equialent
    undefined