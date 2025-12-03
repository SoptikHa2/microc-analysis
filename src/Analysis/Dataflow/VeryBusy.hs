{-# LANGUAGE ScopedTypeVariables #-}
module Analysis.Dataflow.VeryBusy where
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

data VeryBusyLattice a
    = Top
    | Expr (S.Set (Expr a))
    deriving (Eq)

instance Show (VeryBusyLattice a) where
    show Top = "T"
    show (Expr exprs) = intercalate ", " (show <$> S.toList exprs)

instance (Eq a, Ord a) => Lattice (VeryBusyLattice a) where
    top = Top
    bottom = Expr S.empty

    Top <&> x = x
    x <&> Top = x
    (Expr x) <&> (Expr y) = Expr $ S.intersection x y

    Top <|> _ = Top
    _ <|> Top = Top
    (Expr x) <|> (Expr y) = Expr $ S.union x y
    

type VeryBusyResultMap a = ResultMap (VeryBusyLattice a)

solve :: (Data a, Ord a) => CFG a -> VeryBusyResultMap a
solve cfg = runAnalysis runCfg prevId cfg (fromJust $ findExit cfg.idmap).id

containsId :: forall a . (Data a) => Identifier -> Expr a -> Bool
containsId i e = i `elem` usedIds
    where
        usedIds = [i | EIdentifier (_loc :: a) i <- universeBi e]

runCfg :: (Data a, Ord a) => CFGNode a -> State (VeryBusyResultMap a) Bool
runCfg (FunExit nodeId _ retExpr _) = do
    -- We are the first one.
    -- Last line in the function. The only thing that will be used is the return, and nothing else.
    -- We are also able to only be evaluated once, we are the first one to come
    m <- get
    case m M.!? nodeId of
        Nothing -> do
            modify (M.insert nodeId (Expr $ S.fromList [retExpr]))
            pure True
        Just _ -> pure False

runCfg n@(Node nodeId _ _ stmt) = do
    -- Get the following lines.
    -- Things for this node == things for the following node.
    -- HOWEVER:
    -- 1) If we are assignment, we DROP all expressions containing the variable assigned to
    -- 2) If we are if/while/output/assignemnt, we ADD the expression in question
    let shouldKeep = case stmt of
                    AssignmentStmt _ (EIdentifier _ var) _ -> not . containsId var
                    _ -> const True
    let toAdd = case stmt of
                    IfStmt _ cond _ _ -> [cond]
                    WhileStmt _ cond _ -> [cond]
                    OutputStmt _ e -> [e]
                    AssignmentStmt _ _l r -> [r]
                    _ -> []
    
    prevAssignments <- catMaybes <$> gets (\m -> (m M.!?) <$> nextId n)
    let Expr mergedPrev = foldr (<&>) top prevAssignments
    let afterRemoval = S.filter shouldKeep mergedPrev
    let afterAddition = S.union afterRemoval (S.fromList toAdd)
    let mySolution = Expr afterAddition

    existingSolution <- gets (M.!? nodeId)

    let changed = case existingSolution of
                    Just e | e == mySolution -> False
                    _ -> True

    modify (M.insert nodeId mySolution)

    pure changed

runCfg n@(FunEntry nodeId _ _ _ _) = do
    -- In this topmost group, just return whatever was below (it should be empty anyway)
    prevAssignments <- catMaybes <$> gets (\m -> (m M.!?) <$> (nextId n))
    let mergedPrev = foldr (<&>) top prevAssignments

    existingSolution <- gets (M.!? nodeId)

    let changed = case existingSolution of
                    Just e | e == mergedPrev -> False
                    _ -> True
    
    modify (M.insert nodeId mergedPrev)

    pure changed
