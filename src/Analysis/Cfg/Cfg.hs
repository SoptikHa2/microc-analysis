module Analysis.Cfg.Cfg where
import qualified Data.Map as M
import Text.Printf (printf)
import Control.Monad.State (State(..), evalState, get, put)
import Parse.AST

data CFG a = CFG {
    map :: CFGMap a,
    root :: CFGNode a
}

data CFGNode a
    = Node {
          _prev :: [Int],
          _next :: [Int],
          el :: Stmt a
      }
    | FunEntry String Int -- next
    | FunExit String Int -- prev

type CFGMap a = M.Map Int (CFGNode a)

next :: CFGMap a -> CFGNode a -> [CFGNode a]
next m (Node _ nextId _) = (m M.!) <$> nextId
next m (FunEntry _ nextId) = [m M.! nextId]
next _ (FunExit _ _) = []

prev :: CFGMap a -> CFGNode a -> [CFGNode a]
prev m (Node prevId _ _) = (m M.!) <$> prevId
prev m (FunEntry _ _) = []
prev m (FunExit _ prevId) = [m M.! prevId]

cfgshow :: String -> CFG a -> String
cfgshow digraphName (CFG m c) =
    "digraph " <> digraphName <> " {"
        <> evalState (fst <$> printCfg m c) 0
        <> "}"
    where
        gi :: State Int Int
        gi = do
            old <- get
            put (old + 1)
            pure old

        -- returns output string and ID of the topmost node
        printCfg :: CFGMap a -> CFGNode a -> State Int (String, Int)
        printCfg _m (FunExit el _) = do
            i <- gi
            pure (printf "n_%d[label=\"Fun %s exit\"]\n" i (show el), i)
        printCfg m c@(FunEntry label _) = do
            i <- gi
            (child, ci) <- printCfg m nc
            let s = printf "n_%d[label=\"Fun %s entry\"]\n" i label
                    <> child
                    <> printf "n_%d -> n_%d\n\n" i ci
            pure (s, i)
            where
                nc = head (next m c)  -- fun entry -> just one child
        printCfg m c@(Node _ _ el) = do
            i <- gi
            (children, cidx) <- unzip <$> traverse (printCfg m) (next m c)
            let s = printf "n_%d[label=\"%s\"]\n" i (show el)
                    <> foldl (<>) "" children
                    <> foldl (<>) "" (printf "n_%d -> n_%d\n" i <$> cidx)
                    <> "\n"
            pure (s, i)
