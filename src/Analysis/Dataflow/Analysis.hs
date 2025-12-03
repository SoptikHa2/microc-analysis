{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Dataflow.Analysis (ResultMap, ResultLat, runAnalysis, runAnalysisOnVars) where
import qualified Data.Map as M
import Parse.AST (Stmt, Identifier)
import Analysis.Cfg.Cfg
import Lattice (Lattice(..), (<||>))
import Control.Monad.State
import Data.List (nub)
import Data.Maybe (catMaybes)

type ResultMap d = M.Map CFGId d
type ResultLat l = M.Map Identifier l

type FnextIds a = (CFGNode a -> [CFGId])
type FprevIds a = (CFGNode a -> [CFGId])
type FevalStmt a d = (Stmt a -> d -> d)
type FrunCfg a d = (CFGNode a -> State (ResultMap d) Bool)

runAnalysisOnVars ::
    forall a l .
    Lattice l =>
    FnextIds a ->
    FprevIds a ->
    FevalStmt a (ResultLat l) ->
    CFG a -> CFGId -> ResultMap (ResultLat l)
runAnalysisOnVars next prev eval = runAnalysis (runCfg next prev eval) next

runAnalysis ::
    forall a d .
    FrunCfg a d ->
    FnextIds a ->
    CFG a -> CFGId -> ResultMap d -- CFG, first to run (usually root)
runAnalysis runCfg next (CFG cfgMap _) toRun = go cfgMap [toRun] initialResult
    where
        -- Initialize the root node first
        (_, initialResult) = runState (runCfg (cfgMap M.! toRun)) M.empty

        go :: CFGMap a -> [CFGId] -> ResultMap d -> ResultMap d
        go theMap idxs resultMap =
            if null newChanged
                then newResultMap
                else go theMap (nub newChanged) newResultMap
            where
                (newChanged, newResultMap) = foldr
                    (\nodeId (changedIds, res) ->
                        let
                            (newChangedIds, newResult) = runState (runStep runCfg next nodeId theMap) res
                        in
                            (newChangedIds <> changedIds, newResult)
                        )
                    ([], resultMap)
                    idxs

-- Args:
--  id of previously changed CFG node (siblings will be rerun)
--  CFG map of the function
-- State:
--  Mapping from CFG Node -> (Variable -> lattice value)
-- Returns:
--  List of affected CFG nodes
runStep :: FrunCfg a d -> FnextIds a -> CFGId -> CFGMap a -> State (ResultMap d) [Int]
runStep runCfg next toRun cfgMap = do
    -- First, retrieve the CFG
    let cfg = cfgMap M.! toRun
    -- This analysis will run for all children
    let children = (cfgMap M.!) <$> next cfg

    childrenThatChanged <- traverse runCfg children

    -- Return ids of nodes that changed
    let changedChildren = fst <$> filter snd (zip children childrenThatChanged)
    pure $ getId <$> changedChildren

-- Run a single CFG node. Update the 
-- node values in the map if anything changed, and return whether so
runCfg :: Lattice l => FnextIds a -> FprevIds a -> FevalStmt a (ResultLat l) -> CFGNode a -> State (ResultMap (ResultLat l)) Bool
runCfg _next _prev _evalStmt (FunEntry nodeId _ vars args _) = do
    -- The entry is always out-of-date only once
    m <- get
    case m M.!? nodeId of
        Nothing -> do
            let defVars = zip vars (repeat bottom)
            let defArgs = zip args (repeat top)
            let resLat = M.fromList (defVars ++ defArgs)
            modify (M.insert nodeId resLat)
            pure True
        Just _ -> pure False

runCfg _next prev evalStmt n@(Node nodeId _ _ stmt) = do
    -- 1) get assignments of all previous ones
    -- 2) use the statement to compute new constraints
    -- 3) replace self with new ones

    prevAssignments <- catMaybes <$> gets (\m -> (m M.!?) <$> (prev n))
    let mergedPrev = foldr (<||>) M.empty prevAssignments

    -- use the assumptions to compute self values
    let mine = evalStmt stmt mergedPrev

    existingSolution <- gets (M.!? nodeId)

    let changed = case existingSolution of
                Just e | e == mine -> False
                _ -> True

    modify (M.insert nodeId mine)

    pure changed

runCfg _next prev _evalStmt n@(FunExit nodeId _ _ _) = do
    -- 1) get assignments of all previous ones
    -- 2) <|> them
    -- 3) insert into map, return if was equialent

    prevAssignments <- catMaybes <$> gets (\m -> (m M.!?) <$> (prev n))
    let mergedPrev = foldr1 (<||>) prevAssignments

    existingSolution <- gets (M.!? nodeId)

    let changed = case existingSolution of
                Just e | e == mergedPrev -> False
                _ -> True

    modify (M.insert nodeId mergedPrev)

    pure changed
