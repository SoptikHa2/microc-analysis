module StateSpec (spec) where

import Test.Hspec
import Control.Monad.State (StateT, runState, evalState, execState)
import Control.Monad.Identity (Identity)
import qualified Data.Map as M

import Interpreter.State
import Interpreter.Data

-- Helper to run a StateT computation with Identity
runStateTest :: StateT State Identity a -> State -> (a, State)
runStateTest = runState

evalStateTest :: StateT State Identity a -> State -> a
evalStateTest = evalState

execStateTest :: StateT State Identity a -> State -> State
execStateTest = execState

spec :: Spec
spec = do
  describe "empty state" $ do
    it "initializes with an empty stack frame" $
      stack empty `shouldBe` [M.empty]

    it "initializes with an empty heap" $
      heap empty `shouldBe` M.empty

  describe "putsValue" $ do
    it "stores a value on the heap and returns its address" $ do
      let (addr, state') = runStateTest (putsValue (VNumber 42)) empty
      addr `shouldBe` 1
      M.lookup 1 (heap state') `shouldBe` Just (VNumber 42)

    it "increments _nextAddr after storing" $ do
      let state' = execStateTest (putsValue (VNumber 42)) empty
      _nextAddr state' `shouldBe` 2

    it "stores multiple values with sequential addresses" $ do
      let computation = do
            addr1 <- putsValue (VNumber 10)
            addr2 <- putsValue (VNumber 20)
            addr3 <- putsValue (VNumber 30)
            return (addr1, addr2, addr3)
      let ((addr1, addr2, addr3), state') = runStateTest computation empty
      addr1 `shouldBe` 1
      addr2 `shouldBe` 2
      addr3 `shouldBe` 3
      M.lookup 1 (heap state') `shouldBe` Just (VNumber 10)
      M.lookup 2 (heap state') `shouldBe` Just (VNumber 20)
      M.lookup 3 (heap state') `shouldBe` Just (VNumber 30)

  describe "getsAddr" $ do
    it "retrieves a value from the heap by address" $ do
      let state' = execStateTest (putsValue (VNumber 42)) empty
      let result = evalStateTest (getsAddr 1) state'
      result `shouldBe` Just (VNumber 42)

    it "returns Nothing for non-existent address" $ do
      let result = evalStateTest (getsAddr 99) empty
      result `shouldBe` Nothing

  describe "putsAddr" $ do
    it "updates an existing value on the heap" $ do
      let state1 = execStateTest (putsValue (VNumber 42)) empty
      let state2 = execStateTest (putsAddr 0 (VNumber 100)) state1
      M.lookup 0 (heap state2) `shouldBe` Just (VNumber 100)

    it "can insert at a new address" $ do
      let state' = execStateTest (putsAddr 5 (VNumber 99)) empty
      M.lookup 5 (heap state') `shouldBe` Just (VNumber 99)

  describe "putsVar" $ do
    it "stores a variable value on heap and maps it in the stack" $ do
      let state' = execStateTest (putsVar "x" (VNumber 42)) empty
      let topFrame = head (stack state')
      M.lookup "x" topFrame `shouldBe` Just 1
      M.lookup 1 (heap state') `shouldBe` Just (VNumber 42)

    it "updates variable mapping if variable already exists in frame" $ do
      let computation = do
            putsVar "x" (VNumber 10)
            putsVar "x" (VNumber 20)
      let state' = execStateTest computation empty
      let topFrame = head (stack state')
      -- Variable 'x' should point to the same address (1)
      M.lookup "x" topFrame `shouldBe` Just 1
      -- Only the new value should be there, the old one should
      -- be overwritten
      M.lookup 1 (heap state') `shouldBe` Just (VNumber 20)
      M.lookup 2 (heap state') `shouldBe` Nothing

    it "stores multiple variables in the same frame" $ do
      let computation = do
            putsVar "x" (VNumber 10)
            putsVar "y" (VNumber 20)
            putsVar "z" (VNumber 30)
      let state' = execStateTest computation empty
      let topFrame = head (stack state')
      M.lookup "x" topFrame `shouldBe` Just 1
      M.lookup "y" topFrame `shouldBe` Just 2
      M.lookup "z" topFrame `shouldBe` Just 3

  describe "getsVarAddr" $ do
    it "retrieves the address of a variable from the top frame" $ do
      let state' = execStateTest (putsVar "x" (VNumber 42)) empty
      let result = evalStateTest (getsVarAddr "x") state'
      result `shouldBe` Just 1

    it "returns Nothing for non-existent variable" $ do
      let result = evalStateTest (getsVarAddr "nonexistent") empty
      result `shouldBe` Nothing

  describe "getsVar" $ do
    it "retrieves a variable's value from heap via stack mapping" $ do
      let state' = execStateTest (putsVar "x" (VNumber 42)) empty
      let result = evalStateTest (getsVar "x") state'
      result `shouldBe` Just (VNumber 42)

    it "returns Nothing for non-existent variable" $ do
      let result = evalStateTest (getsVar "nonexistent") empty
      result `shouldBe` Nothing

    it "retrieves correct value after variable update" $ do
      let computation = do
            putsVar "x" (VNumber 10)
            putsVar "x" (VNumber 20)
            getsVar "x"
      let result = evalStateTest computation empty
      result `shouldBe` Just (VNumber 20)

  describe "newFrame" $ do
    it "pushes an empty frame onto the stack" $ do
      let state' = execStateTest newFrame empty
      length (stack state') `shouldBe` 2
      head (stack state') `shouldBe` M.empty

    it "preserves existing frames" $ do
      let computation = do
            putsVar "x" (VNumber 10)
            newFrame
      let state' = execStateTest computation empty
      length (stack state') `shouldBe` 2
      -- Old frame should still have 'x'
      M.lookup "x" (stack state' !! 1) `shouldBe` Just 1

    it "new frame does not see variables from previous frame via getsVar" $ do
      let computation = do
            putsVar "x" (VNumber 10)
            newFrame
            getsVar "x"
      let result = evalStateTest computation empty
      result `shouldBe` Nothing

    it "variables in new frame are independent" $ do
      let computation = do
            putsVar "x" (VNumber 10)
            newFrame
            putsVar "x" (VNumber 20)
            x1 <- getsVar "x"
            return x1
      let result = evalStateTest computation empty
      result `shouldBe` Just (VNumber 20)

  describe "dropFrame" $ do
    it "removes the top frame from the stack" $ do
      let computation = do
            newFrame
            dropFrame
      let state' = execStateTest computation empty
      length (stack state') `shouldBe` 1

    it "restores access to previous frame's variables" $ do
      let computation = do
            putsVar "x" (VNumber 10)
            newFrame
            putsVar "y" (VNumber 20)
            dropFrame
            getsVar "x"
      let result = evalStateTest computation empty
      result `shouldBe` Just (VNumber 10)

    it "heap values persist after dropping frame" $ do
      let computation = do
            putsVar "x" (VNumber 10)
            newFrame
            putsVar "y" (VNumber 20)
            dropFrame
      let state' = execStateTest computation empty
      -- Both values should still be on heap
      M.lookup 1 (heap state') `shouldBe` Just (VNumber 10)
      M.lookup 2 (heap state') `shouldBe` Just (VNumber 20)

  describe "frame operations with pointers" $ do
    it "can store and retrieve pointer values" $ do
      let computation = do
            putsVar "x" (VNumber 42)
            addr <- getsVarAddr "x"
            case addr of
              Just a -> putsVar "ptr" (Pointer a)
              Nothing -> return ()
            getsVar "ptr"
      let result = evalStateTest computation empty
      result `shouldBe` Just (Pointer 1)

    it "pointer remains valid across frames" $ do
      let computation = do
            putsVar "x" (VNumber 42)
            addr <- getsVarAddr "x"
            newFrame
            case addr of
              Just a -> do
                putsVar "ptr" (Pointer a)
                val <- getsAddr a
                return val
              Nothing -> return Nothing
      let result = evalStateTest computation empty
      result `shouldBe` Just (VNumber 42)

  describe "complex state operations" $ do
    it "handles multiple frames with shadowing" $ do
      let computation = do
            putsVar "x" (VNumber 1)
            newFrame
            putsVar "x" (VNumber 2)
            newFrame
            putsVar "x" (VNumber 3)
            v3 <- getsVar "x"
            dropFrame
            v2 <- getsVar "x"
            dropFrame
            v1 <- getsVar "x"
            return (v3, v2, v1)
      let result = evalStateTest computation empty
      result `shouldBe` (Just (VNumber 3), Just (VNumber 2), Just (VNumber 1))

    it "maintains correct address allocation across operations" $ do
      let computation = do
            a1 <- putsValue (VNumber 1)
            putsVar "x" (VNumber 2)
            newFrame
            a2 <- putsValue (VNumber 3)
            putsVar "y" (VNumber 4)
            return (a1, a2)
      let ((a1, a2), state') = runStateTest computation empty
      a1 `shouldBe` 1
      a2 `shouldBe` 3
      _nextAddr state' `shouldBe` 5