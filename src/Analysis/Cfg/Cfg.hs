module Analysis.Cfg.Cfg where
import qualified Data.Map as M
import Text.Printf (printf)
import Control.Monad.State (State(..), evalState, get, put)

data CFG
    = CFG {
          _next :: [Int],
          note :: String
      }
    | FunEntry String Int
    | FunExit String

type CFGFun = M.Map Int CFG

next :: CFGFun -> CFG -> [CFG]
next m (CFG nextId _note ) = (m M.!) <$> nextId
next m (FunEntry _ nextId) = [m M.! nextId]
next _ (FunExit _) = []

show :: String -> CFGFun -> CFG -> String
show digraphName m c =
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
        printCfg :: CFGFun -> CFG -> State Int (String, Int)
        printCfg _m (FunExit label) = do
            i <- gi
            pure (printf "n_%d[label=\"Fun %s exit\"]\n" i label, i)
        printCfg m c@(FunEntry label _) = do
            i <- gi
            (child, ci) <- printCfg m nc
            let s = printf "n_%d[label=\"Fun %s entry\"]\n" i label
                    <> child
                    <> printf "n_%d -> n_%d\n\n" i ci
            pure (s, i)
            where
                nc = head (next m c)  -- fun entry -> just one child
        printCfg m c@(CFG _ note) = do
            i <- gi
            (children, cidx) <- unzip <$> traverse (printCfg m) (next m c)
            let s = printf "n_%d[label=\"%s\"]\n" i note
                    <> foldl (<>) "" children
                    <> foldl (<>) "" (printf "n_%d -> n_%d\n" i <$> cidx)
                    <> "\n"
            pure (s, i)
