module InterpretStmtSpec (spec) where

import Test.Hspec
import Control.Monad.State (StateT, runStateT, evalStateT, execStateT, runState, evalState, execState)
import Control.Monad.Identity (Identity)
import qualified Data.Map as M
import Prelude hiding (return)
import qualified Prelude

import Interpreter.Interpret
import Interpreter.State
import Interpreter.Data (Value(..), Address)
import Parse.AST

-- Helper to run a StateT computation with IO
runStmtTest :: StateT State IO a -> State -> IO (a, State)
runStmtTest = runStateT

evalStmtTest :: StateT State IO a -> State -> IO a
evalStmtTest = evalStateT

execStmtTest :: StateT State IO a -> State -> IO State
execStmtTest = execStateT

-- Helper to run a StateT computation with Identity (for state setup)
runStateTest :: StateT State Identity a -> State -> (a, State)
runStateTest = runState

evalStateTest :: StateT State Identity a -> State -> a
evalStateTest = evalState

execStateTest :: StateT State Identity a -> State -> State
execStateTest = execState

-- Helper to get variable value from a state (pure)
getVarFromState :: Identifier -> State -> Maybe Value
getVarFromState var st = evalState (getsVar var) st

spec :: Spec
spec = do
  describe "OutputStmt" $ do
    it "outputs number without error" $ do
      -- Just verify it executes without throwing
      _ <- execStmtTest (evalStmt (OutputStmt (Number 42))) empty
      pure ()

    it "outputs variable value without error" $ do
      let state' = execStateTest (putsVar "x" (VNumber 123)) empty
      _ <- execStmtTest (evalStmt (OutputStmt (EIdentifier "x"))) state'
      pure ()

    it "outputs complex expression without error" $ do
      _ <- execStmtTest (evalStmt (OutputStmt (BiOp Plus (Number 10) (Number 20)))) empty
      pure ()

    it "does not modify state" $ do
      let initialState = execStateTest (putsVar "x" (VNumber 42)) empty
      finalState <- execStmtTest (evalStmt (OutputStmt (EIdentifier "x"))) initialState
      stack initialState `shouldBe` stack finalState
      heap initialState `shouldBe` heap finalState

  describe "AssignmentStmt - identifier target" $ do
    it "assigns number to existing variable" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt (EIdentifier "x") (Number 42))) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 42)

    it "assigns expression result to variable" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt (EIdentifier "x") (BiOp Plus (Number 10) (Number 20)))) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 30)

    it "assigns variable to another variable" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 99)
            putsVar "y" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt (EIdentifier "y") (EIdentifier "x"))) state'
      let result = getVarFromState "y" finalState
      result `shouldBe` Just (VNumber 99)

    it "throws error for undefined variable" $ do
      execStmtTest (evalStmt (AssignmentStmt (EIdentifier "undefined") (Number 42))) empty
        `shouldThrow` anyErrorCall

  describe "AssignmentStmt - dereference target" $ do
    it "assigns through pointer" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 10)
            xAddr <- getsVarAddr "x"
            case xAddr of
              Just a -> putsVar "ptr" (Pointer a)
              Nothing -> Prelude.return ()) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt (UnOp Deref (EIdentifier "ptr")) (Number 42))) state'
      -- Check that dereferencing ptr now gives 42
      result <- evalStmtTest (evalExpr (UnOp Deref (EIdentifier "ptr"))) finalState
      result `shouldBe` VNumber 42

    it "assigns through multiple levels of indirection" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 1)
            xAddr <- getsVarAddr "x"
            case xAddr of
              Just a -> do
                putsVar "ptr1" (Pointer a)
                ptr1Addr <- getsVarAddr "ptr1"
                case ptr1Addr of
                  Just p1 -> putsVar "ptr2" (Pointer p1)
                  Nothing -> Prelude.return ()
              Nothing -> Prelude.return ()) empty
      -- *(*ptr2) = 99
      finalState <- execStmtTest (evalStmt (AssignmentStmt (UnOp Deref (UnOp Deref (EIdentifier "ptr2"))) (Number 99))) state'
      result <- evalStmtTest (evalExpr (EIdentifier "x")) finalState
      result `shouldBe` VNumber 99

  describe "AssignmentStmt - field access target" $ do
    it "assigns to record field" $ do
      let state' = execStateTest (do
            xAddr <- putsValue (VNumber 10)
            yAddr <- putsValue (VNumber 20)
            putsVar "obj" (Interpreter.Data.Record [("x", xAddr), ("y", yAddr)])) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt (FieldAccess (EIdentifier "obj") "x") (Number 999))) state'
      result <- evalStmtTest (evalExpr (FieldAccess (EIdentifier "obj") "x")) finalState
      result `shouldBe` VNumber 999

    it "assigns to one field without affecting other fields" $ do
      let state' = execStateTest (do
            xAddr <- putsValue (VNumber 10)
            yAddr <- putsValue (VNumber 20)
            putsVar "obj" (Interpreter.Data.Record [("x", xAddr), ("y", yAddr)])) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt (FieldAccess (EIdentifier "obj") "x") (Number 100))) state'
      resultX <- evalStmtTest (evalExpr (FieldAccess (EIdentifier "obj") "x")) finalState
      resultY <- evalStmtTest (evalExpr (FieldAccess (EIdentifier "obj") "y")) finalState
      resultX `shouldBe` VNumber 100
      resultY `shouldBe` VNumber 20

  describe "Block" $ do
    it "executes empty block" $ do
      finalState <- execStmtTest (evalStmt (Block [])) empty
      stack finalState `shouldBe` stack empty
      heap finalState `shouldBe` heap empty

    it "executes single statement in block" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (Block [AssignmentStmt (EIdentifier "x") (Number 42)])) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 42)

    it "executes multiple statements in sequence" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 0)
            putsVar "y" (VNumber 0)) empty
      let block = Block [
            AssignmentStmt (EIdentifier "x") (Number 10),
            AssignmentStmt (EIdentifier "y") (Number 20)
            ]
      finalState <- execStmtTest (evalStmt block) state'
      let resultX = getVarFromState "x" finalState
      let resultY = getVarFromState "y" finalState
      resultX `shouldBe` Just (VNumber 10)
      resultY `shouldBe` Just (VNumber 20)

    it "executes statements with dependencies" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 5)
            putsVar "y" (VNumber 0)) empty
      let block = Block [
            AssignmentStmt (EIdentifier "x") (BiOp Plus (EIdentifier "x") (Number 1)),  -- x = x + 1
            AssignmentStmt (EIdentifier "y") (BiOp Mul (EIdentifier "x") (Number 2))    -- y = x * 2
            ]
      finalState <- execStmtTest (evalStmt block) state'
      let resultX = getVarFromState "x" finalState
      let resultY = getVarFromState "y" finalState
      resultX `shouldBe` Just (VNumber 6)
      resultY `shouldBe` Just (VNumber 12)

  describe "IfStmt - without else" $ do
    it "executes then branch when condition is true (non-zero)" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt (Number 1) (AssignmentStmt (EIdentifier "x") (Number 42)) Nothing
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 42)

    it "skips then branch when condition is false (zero)" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt (Number 0) (AssignmentStmt (EIdentifier "x") (Number 42)) Nothing
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 0)

    it "evaluates condition expression correctly" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt (BiOp Gt (Number 5) (Number 3)) (AssignmentStmt (EIdentifier "x") (Number 99)) Nothing
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 99)

  describe "IfStmt - with else" $ do
    it "executes then branch when condition is true" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt (Number 1)
                     (AssignmentStmt (EIdentifier "x") (Number 10))
                     (Just (AssignmentStmt (EIdentifier "x") (Number 20)))
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 10)

    it "executes else branch when condition is false" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt (Number 0)
                     (AssignmentStmt (EIdentifier "x") (Number 10))
                     (Just (AssignmentStmt (EIdentifier "x") (Number 20)))
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 20)

    it "handles pointer as truthy condition" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt (UnOp Alloc (Number 42))
                     (AssignmentStmt (EIdentifier "x") (Number 1))
                     (Just (AssignmentStmt (EIdentifier "x") (Number 0)))
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 1)

    it "handles null pointer as falsy condition" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt Null
                     (AssignmentStmt (EIdentifier "x") (Number 1))
                     (Just (AssignmentStmt (EIdentifier "x") (Number 2)))
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 2)

  describe "WhileStmt" $ do
    it "does not execute when condition is initially false" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let whileStmt = WhileStmt (Number 0) (AssignmentStmt (EIdentifier "x") (Number 42))
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 0)

    it "executes body multiple times while condition is true" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 0)
            putsVar "i" (VNumber 0)) empty
      -- while (i < 3) { i = i + 1; }
      let whileStmt = WhileStmt
            (BiOp Gt (Number 3) (EIdentifier "i"))
            (AssignmentStmt (EIdentifier "i") (BiOp Plus (EIdentifier "i") (Number 1)))
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "i" finalState
      result `shouldBe` Just (VNumber 3)

    it "implements countdown loop" $ do
      let state' = execStateTest (putsVar "n" (VNumber 5)) empty
      -- while (n > 0) { n = n - 1; }
      let whileStmt = WhileStmt
            (BiOp Gt (EIdentifier "n") (Number 0))
            (AssignmentStmt (EIdentifier "n") (BiOp Minus (EIdentifier "n") (Number 1)))
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "n" finalState
      result `shouldBe` Just (VNumber 0)

    it "executes complex body with multiple statements" $ do
      let state' = execStateTest (do
            putsVar "i" (VNumber 0)
            putsVar "sum" (VNumber 0)) empty
      -- while (i < 5) { sum = sum + i; i = i + 1; }
      let whileStmt = WhileStmt
            (BiOp Gt (Number 5) (EIdentifier "i"))
            (Block [
              AssignmentStmt (EIdentifier "sum") (BiOp Plus (EIdentifier "sum") (EIdentifier "i")),
              AssignmentStmt (EIdentifier "i") (BiOp Plus (EIdentifier "i") (Number 1))
            ])
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let resultSum = getVarFromState "sum" finalState
      let resultI = getVarFromState "i" finalState
      resultSum `shouldBe` Just (VNumber 10)  -- 0+1+2+3+4 = 10
      resultI `shouldBe` Just (VNumber 5)

  describe "Complex statement combinations" $ do
    it "nested if statements" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let nestedIf = IfStmt (Number 1)
                       (IfStmt (Number 1)
                         (AssignmentStmt (EIdentifier "x") (Number 42))
                         Nothing)
                       Nothing
      finalState <- execStmtTest (evalStmt nestedIf) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 42)

    it "while loop with if inside" $ do
      let state' = execStateTest (do
            putsVar "i" (VNumber 0)
            putsVar "evens" (VNumber 0)) empty
      -- Count even numbers from 0 to 4
      -- while (i < 5) { if (i % 2 == 0) evens = evens + 1; i = i + 1; }
      let whileStmt = WhileStmt
            (BiOp Gt (Number 5) (EIdentifier "i"))
            (Block [
              -- Skip modulo, just check if i is 0, 2, or 4 manually
              IfStmt (BiOp Eq (EIdentifier "i") (Number 0))
                (AssignmentStmt (EIdentifier "evens") (BiOp Plus (EIdentifier "evens") (Number 1)))
                Nothing,
              IfStmt (BiOp Eq (EIdentifier "i") (Number 2))
                (AssignmentStmt (EIdentifier "evens") (BiOp Plus (EIdentifier "evens") (Number 1)))
                Nothing,
              IfStmt (BiOp Eq (EIdentifier "i") (Number 4))
                (AssignmentStmt (EIdentifier "evens") (BiOp Plus (EIdentifier "evens") (Number 1)))
                Nothing,
              AssignmentStmt (EIdentifier "i") (BiOp Plus (EIdentifier "i") (Number 1))
            ])
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "evens" finalState
      result `shouldBe` Just (VNumber 3)

    it "if-else chain" $ do
      let state' = execStateTest (do
            putsVar "grade" (VNumber 85)
            putsVar "letter" (VNumber 0)) empty
      -- Simplified grade assignment
      let ifChain = IfStmt (BiOp Gt (EIdentifier "grade") (Number 90))
                      (AssignmentStmt (EIdentifier "letter") (Number 4))  -- A
                      (Just (IfStmt (BiOp Gt (EIdentifier "grade") (Number 80))
                        (AssignmentStmt (EIdentifier "letter") (Number 3))  -- B
                        (Just (AssignmentStmt (EIdentifier "letter") (Number 2)))))  -- C
      finalState <- execStmtTest (evalStmt ifChain) state'
      let result = getVarFromState "letter" finalState
      result `shouldBe` Just (VNumber 3)

    it "assignment with function call on right side" $ do
      let funDecl = FunDecl "getVal" [] (FunBlock [] [] (Number 123))
      let state' = execStateTest (do
            putsVar "f" (Function funDecl)
            putsVar "x" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt (EIdentifier "x") (Call (EIdentifier "f") []))) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 123)

    it "pointer manipulation in loop" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 0)
            xAddr <- getsVarAddr "x"
            case xAddr of
              Just a -> putsVar "ptr" (Pointer a)
              Nothing -> Prelude.return ()
            putsVar "i" (VNumber 0)) empty
      -- while (i < 3) { *ptr = *ptr + 1; i = i + 1; }
      let whileStmt = WhileStmt
            (BiOp Gt (Number 3) (EIdentifier "i"))
            (Block [
              AssignmentStmt (UnOp Deref (EIdentifier "ptr"))
                (BiOp Plus (UnOp Deref (EIdentifier "ptr")) (Number 1)),
              AssignmentStmt (EIdentifier "i") (BiOp Plus (EIdentifier "i") (Number 1))
            ])
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 3)
