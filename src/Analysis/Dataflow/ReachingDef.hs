{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Dataflow.ReachingDef where
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

data ReachingDefLattice a
    = Top
    | Stmtx (S.Set (Stmt a))
    deriving (Eq)

instance Show (ReachingDefLattice a) where
    show Top = "T"
    show (Stmtx stmtx) = intercalate ", " (show <$> S.toList stmtx)

instance (Eq a, Ord a) => Lattice (ReachingDefLattice a) where
    top = Top
    bottom = Stmtx S.empty

    Top <&> x = x
    x <&> Top = x
    (Stmtx x) <&> (Stmtx y) = Stmtx $ S.intersection x y

    Top <|> _ = Top
    _ <|> Top = Top
    (Stmtx x) <|> (Stmtx y) = Stmtx $ S.union x y
    

type ReachingDefResultMap a = ResultMap (ReachingDefLattice a)

solve :: (Data a, Ord a) => CFG a -> ReachingDefResultMap a
solve cfg = runAnalysis runCfg prevId cfg (fromJust $ findExit cfg.idmap).id

containsId :: forall a . (Data a) => Identifier -> Expr a -> Bool
containsId i e = i `elem` usedIds
    where
        usedIds = [i | EIdentifier (_loc :: a) i <- universeBi e]

isInvalid :: forall a . (Data a) => Expr a -> Bool
isInvalid e = any Prelude.id invalidSub
    where
        invalidSub = [
                True | Input (_ :: a) <- universeBi e
            ]

runCfg :: (Data a, Ord a) => CFGNode a -> State (ReachingDefResultMap a) Bool
runCfg (FunEntry nodeId _ _ _ _) = do
    -- We are the first one.
    -- At the beginning, there are no assignments. So nothing to have here
    m <- get
    case m M.!? nodeId of
        Nothing -> do
            modify (M.insert nodeId bottom)
            pure True
        Just _ -> pure False

runCfg n@(Node nodeId _ _ stmt) = do
    -- We are looking for stuff that might have defined a variable.
    -- 1) Previous values will be added using <|>
    -- 2) If we are assignment, drop assignments to the same variable from parents
    -- 3) Add together this assignment and the parents
    prevAssignments <- catMaybes <$> gets (\m -> (m M.!?) <$> nextId n)
    let Stmtx mergedPrev = foldr (<|>) bottom prevAssignments
    
    let (namesToDrop, assignments) = case stmt of
                    AssignmentStmt _ (EIdentifier _ var) _ -> ([var], [stmt])
                    _ -> ([],[])

    let shouldKeep s = case s of
            AssignmentStmt _ (EIdentifier _ var) _ -> var `notElem` namesToDrop
            _ -> False
    
    let prevAfterDrop = S.filter shouldKeep mergedPrev
    let mySolution = Stmtx (prevAfterDrop) <|> Stmtx (S.fromList assignments)

    existingSolution <- gets (M.!? nodeId)

    let changed = case existingSolution of
                    Just e | e == mySolution -> False
                    _ -> True

    modify (M.insert nodeId mySolution)

    pure changed

runCfg n@(FunExit nodeId _ _ _) = do
    -- No change here, just return the previous ones
    prevAssignments <- catMaybes <$> gets (\m -> (m M.!?) <$> (nextId n))
    let mergedPrev = foldr (<|>) bottom prevAssignments

    existingSolution <- gets (M.!? nodeId)

    let changed = case existingSolution of
                    Just e | e == mergedPrev -> False
                    _ -> True
    
    modify (M.insert nodeId mergedPrev)

    pure changed
