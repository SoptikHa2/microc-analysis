module Analysis.Cfg.Cfg where
import qualified Data.Map as M
import Text.Printf (printf)
import Parse.AST

data CFG a = CFG {
    map :: CFGMap a,
    root :: CFGNode a
}

data CFGNode a
    = Node {
        id :: Int,
        _prev :: [Int],
        _next :: [Int],
        el :: Stmt a
      }
    | FunEntry {
        id :: Int,
        funName :: String,
        _next :: [Int]
      }
    | FunExit {
        id :: Int,
        funName :: String,
        _prev :: [Int]
      }

type CFGMap a = M.Map Int (CFGNode a)

addToPrev :: CFGNode a -> Int -> CFGNode a
addToPrev (Node id p n el) i = Node id (i:p) n el
addToPrev n@(FunEntry _ _ _) _ = n
addToPrev (FunExit id s _) i = FunExit id s i

addToNext :: CFGNode a -> Int -> CFGNode a
addToNext (Node id p n el) i = Node id p (i:n) el
addToNext (FunEntry id s _) i = FunEntry id s i
addToNext n@(FunExit _ _ _) _ = n

setId :: CFGNode a -> Int -> CFGNode a
setId (Node _ a b c) i = Node i a b c
setId (FunEntry _ a b) i = FunEntry i a b
setId (FunExit _ a b) i = FunExit i a b

getId :: CFGNode a -> Int
getId (Node i _ _ _) = i
getId (FunEntry i _ _) = i
getId (FunExit i _ _) = i

next :: CFGMap a -> CFGNode a -> [CFGNode a]
next m (Node _ _ nextId _) = (m M.!) <$> nextId
next m (FunEntry _ _ nextId) = (m M.!) <$> nextId
next _ (FunExit _ _ _) = []

prev :: CFGMap a -> CFGNode a -> [CFGNode a]
prev m (Node _ prevId _ _) = (m M.!) <$> prevId
prev m (FunEntry _ _ _) = []
prev m (FunExit _ _ prevId) = (m M.!) <$> prevId

cfgshow :: String -> CFG a -> String
cfgshow digraphName (CFG m c) =
    "digraph " <> digraphName <> " {"
        <> fst (printCfg m c)
        <> "}"
    where
        -- returns output string and ID of the topmost node
        printCfg :: CFGMap a -> CFGNode a -> (String, Int)
        printCfg _m (FunExit i el _) =
            (printf "n_%d[label=\"Fun %s exit\"]\n" i (show el), i)
        printCfg m c@(FunEntry i label _) = (s, i)
            where
                (child, ci) = printCfg m nc
                s = printf "n_%d[label=\"Fun %s entry\"]\n" i label
                    <> child
                    <> printf "n_%d -> n_%d\n\n" i ci
                nc = head (next m c)  -- fun entry -> just one child
        printCfg m c@(Node i _ _ el) = (s, i)
            where
                (children, cidx) = unzip $ printCfg m <$> next m c
                s = printf "n_%d[label=\"%s\"]\n" i (show el)
                    <> foldl (<>) "" children
                    <> foldl (<>) "" (printf "n_%d -> n_%d\n" i <$> cidx)
                    <> "\n"
