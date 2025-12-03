module Analysis.Cfg.Builder where
import Analysis.Cfg.Cfg
import Parse.AST
import Control.Monad.State (State, get, modify, gets, evalState)
import qualified Data.Map as M
import Control.Monad
import Data.Foldable

build :: FunDecl a -> CFG a
build fun = evalState (buildFun fun) M.empty

buildFun :: FunDecl a -> State (CFGMap a) (CFG a)
buildFun fun = do
    funStartNode <- genId $ FunEntry 0 fun.name fun.body.idDecl []
    (bodyFirst, bodyLast) <- buildStmt (Block fun.body.d fun.body.body)
    funEndNode <- genId $ FunExit 0 fun.name (show fun.body.return) []

    addChild funStartNode.id bodyFirst.id
    traverse_ (\p -> addChild p.id funEndNode.id) bodyLast

    rootNode <- refresh funStartNode
    m <- get
    pure $ CFG m rootNode

genId :: CFGNode a -> State (CFGMap a) (CFGNode a)
genId node = do
    nextId <- gets M.size
    let nodeWithId = setId node nextId
    modify (M.insert nextId nodeWithId)
    pure nodeWithId

addChildren :: CFGId -> [CFGId] -> State (CFGMap a) ()
addChildren parent children = forM_ children (addChild parent)

-- For a node [Id], add child [Id]
addChild :: CFGId -> CFGId -> State (CFGMap a) ()
addChild parent child = do
    parentNode <- gets (M.! parent)
    childNode <- gets (M.! child)

    let newParent = addToNext parentNode child
    let newChild = addToPrev childNode parent

    modify (M.insert parent newParent)
    modify (M.insert child newChild)

asSingleCfgNode :: CFGNode a -> (CFGNode a, [CFGNode a])
asSingleCfgNode n = (n, [n])

refresh :: CFGNode a -> State (CFGMap a) (CFGNode a)
refresh node = gets (M.! node.id)

newNode :: Stmt a -> CFGNode a
newNode = Node 0 [] []


-- Returns first and last blocks of generated sequence
buildStmt :: Stmt a -> State (CFGMap a) (CFGNode a, [CFGNode a])
buildStmt s@(OutputStmt _ _) = asSingleCfgNode <$> genId (newNode s)
buildStmt s@(AssignmentStmt _ _ _) = asSingleCfgNode <$> genId (newNode s)
buildStmt s@(Block _ stmx) = do
    basicBlocks <- traverse buildStmt stmx

    let bbHeads = fst <$> basicBlocks
    let bbTails = snd <$> basicBlocks
    -- The blocks themselves are not linked together yet,
    -- but each of them may have multiple children.
    -- Connect each block's tail to the next block's head
    let blockHeadIds = getId <$> bbHeads
    let blockTailIds = fmap (fmap getId) bbTails
    zipWithM_ (\tails nextHead -> forM_ tails (\tailId -> addChild tailId nextHead)) blockTailIds (tail blockHeadIds)

    -- Return toplevel block, and all bottom-level blocks
    firstOfSeq <- gets (M.! head blockHeadIds)
    lastOfSeq <- gets (\m -> (M.!) m <$> last blockTailIds)

    pure (firstOfSeq, lastOfSeq)
buildStmt s@(WhileStmt _ _ stmt) = do
    whileCfg <- genId $ newNode s

    (whileBodyStart, whileBodyEnds) <- buildStmt stmt

    -- Cfg goes to while body
    addChild whileCfg.id whileBodyStart.id
    -- End of body goes to the while loop cfg
    traverse_ (\parent -> addChild parent whileCfg.id) (getId <$> whileBodyEnds)

    cfgStart <- refresh whileCfg

    pure (cfgStart, [cfgStart])
buildStmt s@(IfStmt _ _ tru Nothing) = do
    ifCfg <- genId $ newNode s

    (truStart, truEnd) <- buildStmt tru
    addChild ifCfg.id truStart.id

    cfgStart <- refresh ifCfg
    cfgEnd <- traverse refresh truEnd

    pure (cfgStart, cfgStart:cfgEnd)
buildStmt s@(IfStmt _ _ tru (Just fals)) = do
    ifCfg <- genId $ newNode s

    (truStart, truEnd) <- buildStmt tru
    (falsStart, falsEnd) <- buildStmt fals

    addChild ifCfg.id truStart.id
    addChild ifCfg.id falsStart.id

    cfgStart <- refresh ifCfg
    cfgEnd <- traverse refresh (truEnd ++ falsEnd)

    pure (cfgStart, cfgEnd)
