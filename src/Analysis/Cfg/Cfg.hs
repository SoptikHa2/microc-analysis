module Analysis.Cfg.Cfg where
import qualified Data.Map as M
import Text.Printf (printf)
import Parse.AST
import Data.List (intercalate)
import Control.Monad.State

type CFGId = Int

-- CFG of a single function
data CFG a = CFG {
    idmap :: CFGMap a,
    root :: CFGNode a
}

data CFGNode a
    = Node {
        id :: CFGId,
        _prev :: [CFGId],
        _next :: [CFGId],
        el :: Stmt a
      }
    | FunEntry {
        id :: CFGId,
        funName :: String,
        funVars :: [String],
        _next :: [CFGId]
      }
    | FunExit {
        id :: CFGId,
        funName :: String,
        retVal :: String,
        _prev :: [CFGId]
      }
      deriving (Show)

type CFGMap a = M.Map CFGId (CFGNode a)

addToPrev :: CFGNode a -> CFGId -> CFGNode a
addToPrev (Node id p n el) i = Node id (i:p) n el
addToPrev n@(FunEntry _ _ _ _) _ = n
addToPrev (FunExit id s r p) i = FunExit id s r (i:p)

addToNext :: CFGNode a -> CFGId -> CFGNode a
addToNext (Node id p n el) i = Node id p (i:n) el
addToNext (FunEntry id s v n) i = FunEntry id s v (i:n)
addToNext n@(FunExit _ _ _ _) _ = n

setId :: CFGNode a -> CFGId -> CFGNode a
setId (Node _ a b c) i = Node i a b c
setId (FunEntry _ a b c) i = FunEntry i a b c
setId (FunExit _ a b c) i = FunExit i a b c

getId :: CFGNode a -> CFGId
getId (Node i _ _ _) = i
getId (FunEntry i _ _ _) = i
getId (FunExit i _ _ _) = i

next :: CFGMap a -> CFGNode a -> [CFGNode a]
next m (Node _ _ nextId _) = (m M.!) <$> nextId
next m (FunEntry _ _ _ nextId) = (m M.!) <$> nextId
next _ (FunExit _ _ _ _) = []

prev :: CFGMap a -> CFGNode a -> [CFGNode a]
prev m (Node _ prevId _ _) = (m M.!) <$> prevId
prev m (FunEntry _ _ _ _) = []
prev m (FunExit _ _ _ prevId) = (m M.!) <$> prevId

cfgshow :: String -> CFG a -> String
cfgshow digraphName (CFG m c) =
    "digraph " <> digraphName <> " {\n"
        <> fst (evalState (printCfg m c) [])
        <> "}"
    where
        printCfg :: CFGMap a -> CFGNode a -> State [Int] (String, CFGId)
        printCfg m node = do
            wasGenerated <- gets (node.id `elem`)

            if wasGenerated
                then pure ("", node.id)
                else do
                    modify (node.id :)
                    (res, ri) <- go m node
                    pure (res, ri)

        -- returns output string and ID of the topmost node
        go :: CFGMap a -> CFGNode a -> State [Int] (String, CFGId)
        go _m (FunExit i el retVal _) = pure (retLabel <> exitLabel <> retTransition, i)
            where
                retLabel = printf "n_%d[label=\"return %s;\"]\n" i retVal
                exitLabel = printf "n_exit[label=\"Fun %s exit\"]\n" el
                retTransition = printf "n_%d -> n_exit\n" i
        go m c@(FunEntry i label vars _) = do
            let nc = head (next m c)  -- fun entry -> just one child
            (child, ci) <- printCfg m nc

            let varIds = (++"var") . show <$> take (length vars) [1..]

            let s = printf "n_%d[label=\"Fun %s entry\"]\n" i label
                    <> genVars (zip vars varIds) i ci
                    <> child

            pure (s, i)
        go m c@(Node i _ _ el) = do
            (children, cidx) <- unzip <$> traverse (printCfg m) (next m c)
            let s = printf "n_%d[label=\"%s\"]\n" i (show el)
                    <> foldl (<>) "" children
                    <> foldl (<>) "" (printf "n_%d -> n_%d\n" i <$> cidx)
            pure (s, i)

        -- var; var id; start; end
        genVars :: [(String, String)] -> CFGId -> CFGId -> String
        genVars varsZ startId endId = intercalate "\n" (nodes <> transitionsStr)
            where
                (vars, varIds) = unzip varsZ
                idLine = show startId : varIds ++ [show endId]
                transitions = zip idLine (tail idLine)
                transitionsStr =
                    (\(f, t) -> printf "n_%s -> n_%s\n" f t :: String) <$> transitions
                nodes =
                    (\(v, i) ->
                        printf "n_%s[label=\"var %s;\"]\n" i v :: String) <$> varsZ
