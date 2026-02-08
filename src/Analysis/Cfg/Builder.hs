module Analysis.Cfg.Builder (build, buildStmt, buildWithMap, BuilderState, emptyState, cfgMap) where
import Analysis.Cfg.Cfg
import Parse.AST
import Control.Monad.State (State, get, modify, gets, evalState)
import qualified Data.Map as M
import Control.Monad
import Data.Foldable

-- Builder state: CFG nodes + stmt annotation -> CFGId mapping
data BuilderState a = BuilderState {
    cfgMap :: CFGMap a,
    stmtCfgMap :: StmtCfgMap a
}

emptyState :: BuilderState a
emptyState = BuilderState M.empty M.empty

build :: Ord a => FunDecl a -> CFG a
build fun = cfg $ buildWithMap fun

buildWithMap :: Ord a => FunDecl a -> CFGWithMap a
buildWithMap fun = evalState (buildFun fun) emptyState

buildFun :: Ord a => FunDecl a -> State (BuilderState a) (CFGWithMap a)
buildFun fun = do
    funStartNode <- genId $ FunEntry 0 fun.name fun.body.idDecl fun.args []
    funEndNode <- genId $ FunExit 0 fun.name fun.body.return []

    case fun.body.body of
        [] -> addChild funStartNode.id funEndNode.id  -- empty body: entry -> exit
        stmts -> do
            (bodyFirst, bodyLast) <- buildStmt (Block fun.body.d stmts)
            addChild funStartNode.id bodyFirst.id
            traverse_ (\p -> addChild p.id funEndNode.id) bodyLast

    rootNode <- refresh funStartNode
    st <- get
    pure $ CFGWithMap (CFG st.cfgMap rootNode) st.stmtCfgMap

genId :: CFGNode a -> State (BuilderState a) (CFGNode a)
genId node = do
    nextId <- gets (M.size . cfgMap)
    let nodeWithId = setId node nextId
    modify (\s -> s { cfgMap = M.insert nextId nodeWithId s.cfgMap })
    pure nodeWithId

-- Record mapping from stmt annotation to CFG ID
recordStmt :: Ord a => a -> CFGId -> State (BuilderState a) ()
recordStmt ann cfgId = modify (\s -> s { stmtCfgMap = M.insert ann cfgId s.stmtCfgMap })

-- For a node [Id], add child [Id]
addChild :: CFGId -> CFGId -> State (BuilderState a) ()
addChild parent child = do
    parentNode <- gets ((M.! parent) . cfgMap)
    childNode <- gets ((M.! child) . cfgMap)

    let newParent = addToNext parentNode child
    let newChild = addToPrev childNode parent

    modify (\s -> s { cfgMap = M.insert parent newParent s.cfgMap })
    modify (\s -> s { cfgMap = M.insert child newChild s.cfgMap })

asSingleCfgNode :: CFGNode a -> (CFGNode a, [CFGNode a])
asSingleCfgNode n = (n, [n])

refresh :: CFGNode a -> State (BuilderState a) (CFGNode a)
refresh node = gets ((M.! node.id) . cfgMap)

newNode :: Stmt a -> CFGNode a
newNode = Node 0 [] []

-- Generate node and record the stmt -> CFGId mapping
genStmtNode :: Ord a => Stmt a -> State (BuilderState a) (CFGNode a)
genStmtNode s = do
    node <- genId (newNode s)
    recordStmt (stmtData s) node.id
    pure node

-- Returns first and last blocks of generated sequence
buildStmt :: Ord a => Stmt a -> State (BuilderState a) (CFGNode a, [CFGNode a])
buildStmt s@(OutputStmt _ _) = asSingleCfgNode <$> genStmtNode s
buildStmt s@(AssignmentStmt _ _ _) = asSingleCfgNode <$> genStmtNode s
buildStmt s@(Block _ []) = asSingleCfgNode <$> genStmtNode s  -- empty block
buildStmt _s@(Block _ stmx) = do
    basicBlocks <- traverse buildStmt stmx

    let bbHeads = fst <$> basicBlocks
    let bbTails = snd <$> basicBlocks
    -- The blocks themselves are not linked together yet,
    -- but each of them may have multiple children.
    -- Connect each block's tail to the next block's head
    let blockHeadIds = getId <$> bbHeads
    let blockTailIds = fmap (fmap getId) bbTails
    zipWithM_ (\tails nextHead -> forM_ tails (`addChild` nextHead)) blockTailIds (tail blockHeadIds)

    -- Return toplevel block, and all bottom-level blocks
    firstOfSeq <- gets ((M.! head blockHeadIds) . cfgMap)
    lastOfSeq <- gets (\s -> (M.!) s.cfgMap <$> last blockTailIds)

    pure (firstOfSeq, lastOfSeq)
buildStmt s@(WhileStmt _ _ stmt) = do
    whileCfg <- genStmtNode s

    (whileBodyStart, whileBodyEnds) <- buildStmt stmt

    -- Cfg goes to while body
    addChild whileCfg.id whileBodyStart.id
    -- End of body goes to the while loop cfg
    traverse_ (\parent -> addChild parent whileCfg.id) (getId <$> whileBodyEnds)

    cfgStart <- refresh whileCfg

    pure (cfgStart, [cfgStart])
buildStmt s@(IfStmt _ _ tru Nothing) = do
    ifCfg <- genStmtNode s

    (truStart, truEnd) <- buildStmt tru
    addChild ifCfg.id truStart.id

    cfgStart <- refresh ifCfg
    cfgEnd <- traverse refresh truEnd

    pure (cfgStart, cfgStart:cfgEnd)
buildStmt s@(IfStmt _ _ tru (Just fals)) = do
    ifCfg <- genStmtNode s

    (truStart, truEnd) <- buildStmt tru
    (falsStart, falsEnd) <- buildStmt fals

    addChild ifCfg.id truStart.id
    addChild ifCfg.id falsStart.id

    cfgStart <- refresh ifCfg
    cfgEnd <- traverse refresh (truEnd ++ falsEnd)

    pure (cfgStart, cfgEnd)
