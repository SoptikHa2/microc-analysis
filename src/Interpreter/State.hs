module Interpreter.State (State(..), empty, getsVar, getsAddr, getsVarAddr, putsVar, putsAddr, putsValue, newFrame, dropFrame) where
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
-- Start with address = 1, so we have nullptr = 0
empty = State { stack = [M.empty], heap = M.empty, _nextAddr = 1 }

-- heh, everything is on heap, so we can do pointers easily
type Stack = M.Map Identifier Address

-- Retrieve variable value from stack
getsVar :: Identifier -> StateT State Identity (Maybe Value)
getsVar id = do
    addr <- getsVarAddr id
    join <$> traverse getsAddr addr

-- Retrieve address of a variable
getsVarAddr :: Identifier -> StateT State Identity (Maybe Address)
getsVarAddr id = do
    s <- gets $ head . stack
    pure $ M.lookup id s

-- Dereference address into heap, and read
getsAddr :: Address -> StateT State Identity (Maybe Value)
getsAddr addr = gets (M.lookup addr . heap)

-- Create or update variable in current stack top
putsVar :: Identifier -> Value -> StateT State Identity ()
putsVar id val = do
    existingAddr <- getsVarAddr id

    case existingAddr of
        Just addr -> do
            -- Update the heap and do nothing with the stack
            putsAddr addr val
            pure ()
        Nothing -> do
            -- First, insert onto heap
            addr <- putsValue val
            -- Now, put the result onto stack
            st <- gets stack
            let newHead = M.insert id addr (head st)
            let newStack = newHead : tail st

            modify (\(State _ h na) -> State { stack = newStack, heap = h, _nextAddr = na })

-- Dereference address into heap, and write
putsAddr :: Address -> Value -> StateT State Identity ()
putsAddr addr val = modify (\(State s h na) -> State s (M.insert addr val h) na)

-- Allocate space for new value, save it there, and return the new address.
putsValue :: Value -> StateT State Identity Address
putsValue val = do
    h <- gets heap
    oldNa <- gets _nextAddr
    let newNa = oldNa + 1
    let newH = M.insert oldNa val h

    modify (\(State s _ _) -> State s newH newNa)
    pure oldNa

-- Create new stack frame
newFrame :: StateT State Identity ()
newFrame = modify (\(State s h na) -> State (M.empty : s) h na)

-- Drop top stack frame
dropFrame :: StateT State Identity ()
-- TODO: Cleanup memory
-- pointers will be dangling, and it's okay
dropFrame = modify (\(State s h na) -> State (tail s) h na)
