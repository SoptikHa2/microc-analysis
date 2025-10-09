module Interpreter.State where
import Interpreter.Data
import Parse.AST
import Control.Monad.State (StateT, gets, modify)
import Control.Monad.Identity (Identity)
import Control.Monad
import qualified Data.Map as M

data State = State {
    stack :: [Stack],
    heap :: M.Map Address Value,
    _nextAddr :: Address
}

empty :: State
empty = State { stack = [M.empty], heap = M.empty, _nextAddr = 0 }

-- heh, everything is on heap, so we can do pointers easily
type Stack = M.Map Identifier Address

getsVar :: Identifier -> StateT State Identity (Maybe Value)
getsVar id = do
    addr <- getsVarAddr id
    join <$> traverse getsAddr addr

getsVarAddr :: Identifier -> StateT State Identity (Maybe Address)
getsVarAddr id = do
    s <- gets $ head . stack
    pure $ M.lookup id s

getsAddr :: Address -> StateT State Identity (Maybe Value)
getsAddr addr = gets (M.lookup addr . heap)

putsVar :: Identifier -> Value -> StateT State Identity ()
putsVar id val = do
    -- First, insert onto heap
    addr <- putsValue val
    st <- gets stack
    let newHead = M.insert id addr (head st)
    let newStack = newHead : tail st

    modify (\(State _ h na) -> State newStack h na)

putsAddr :: Address -> Value -> StateT State Identity ()
putsAddr addr val = modify (\(State s h na) -> State s (M.insert addr val h) na)

putsValue :: Value -> StateT State Identity Address
putsValue val = do
    h <- gets heap
    oldNa <- gets _nextAddr
    let newNa = oldNa + 1
    let newH = M.insert oldNa val h

    modify (\(State s _ _) -> State s newH newNa)
    pure oldNa

newFrame :: StateT State Identity ()
newFrame = modify (\(State s h na) -> State ([] <> s) h na)

dropFrame :: StateT State Identity ()
dropFrame = modify (\(State s h na) -> State (tail s) h na)
