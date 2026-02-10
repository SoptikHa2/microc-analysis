{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Dataflow.Liveness (
    solve,
    LivenessLattice(..),
    LivenessResultMap
) where
import Analysis.Dataflow.Analysis (ResultMap, runAnalysis)
import qualified Data.Set as S
import Parse.AST (Expr (..), Stmt (..), Identifier)
import Analysis.Cfg.Cfg
import Data.Maybe (fromJust, catMaybes)
import Control.Monad.State
import Data.List (intercalate)
import Lattice (Lattice(..))
import qualified Data.Map as M
import Data.Generics.Uniplate.Data (universeBi)
import Data.Data (Data)

data LivenessLattice
    = Top
    | Live (S.Set Identifier)
    deriving (Eq)

instance Show LivenessLattice where
    show Top = "T"
    show (Live vars) = intercalate ", " (S.toList vars)

instance Lattice LivenessLattice where
    top = Top
    bottom = Live S.empty

    Top <&> x = x
    x <&> Top = x
    (Live x) <&> (Live y) = Live $ S.intersection x y

    Top <|> _ = Top
    _ <|> Top = Top
    (Live x) <|> (Live y) = Live $ S.union x y

type LivenessResultMap = ResultMap LivenessLattice

solve :: Data a => CFG a -> LivenessResultMap
solve cfg = runAnalysis runCfg prevId cfg (fromJust $ findExit cfg.idmap).id

freeVars :: forall a. Data a => Expr a -> S.Set Identifier
freeVars e = S.fromList [i | EIdentifier (_ :: a) i <- universeBi e]

runCfg :: Data a => CFGNode a -> State LivenessResultMap Bool
runCfg (FunExit nodeId _ retExpr _) = do
    m <- get
    case m M.!? nodeId of
        Nothing -> do
            modify (M.insert nodeId (Live $ freeVars retExpr))
            pure True
        Just _ -> pure False

runCfg n@(Node nodeId _ _ stmt) = do
    succStates <- catMaybes <$> gets (\m -> (m M.!?) <$> nextId n)
    let liveOut = case foldr (<|>) bottom succStates of
            Live s -> s
            Top -> error "Liveness: unexpected Top"

    let (kill, gen) = case stmt of
            AssignmentStmt _ (EIdentifier _ var) rhs -> (S.singleton var, freeVars rhs)
            AssignmentStmt _ lhs rhs -> (S.empty, freeVars lhs `S.union` freeVars rhs)
            IfStmt _ cond _ _ -> (S.empty, freeVars cond)
            WhileStmt _ cond _ -> (S.empty, freeVars cond)
            OutputStmt _ e -> (S.empty, freeVars e)
            _ -> (S.empty, S.empty)

    let mySolution = Live $ (liveOut `S.difference` kill) `S.union` gen

    existingSolution <- gets (M.!? nodeId)
    let changed = case existingSolution of
            Just e | e == mySolution -> False
            _ -> True

    modify (M.insert nodeId mySolution)
    pure changed

runCfg n@(FunEntry nodeId _ _ _ _) = do
    succStates <- catMaybes <$> gets (\m -> (m M.!?) <$> nextId n)
    let mergedSucc = foldr (<|>) bottom succStates

    existingSolution <- gets (M.!? nodeId)
    let changed = case existingSolution of
            Just e | e == mergedSucc -> False
            _ -> True

    modify (M.insert nodeId mergedSucc)
    pure changed
