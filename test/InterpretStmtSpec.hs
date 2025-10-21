module InterpretStmtSpec (spec) where

import Test.Hspec
import Control.Monad.State (StateT, evalStateT, execStateT, runState, evalState, execState)
import Control.Monad.Identity (Identity)
import Prelude hiding (return)
import qualified Prelude

import Interpreter.Interpret
import Interpreter.State
import Interpreter.Data (Value(..))
import Parse.AST
import Text.Parsec.Pos (SourcePos, newPos)

-- Helper to run a StateT computation with IO
-- Helper function to create a dummy SourcePos for testing
testPos :: SourcePos
testPos = newPos "test" 0 0

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
      _ <- execStmtTest (evalStmt (OutputStmt  testPos (Number  testPos 42))) empty
      pure ()

    it "outputs variable value without error" $ do
      let state' = execStateTest (putsVar "x" (VNumber 123)) empty
      _ <- execStmtTest (evalStmt (OutputStmt  testPos (EIdentifier  testPos "x"))) state'
      pure ()

    it "outputs complex expression without error" $ do
      _ <- execStmtTest (evalStmt (OutputStmt  testPos (BiOp  testPos Plus (Number  testPos 10) (Number  testPos 20)))) empty
      pure ()

    it "does not modify state" $ do
      let initialState = execStateTest (putsVar "x" (VNumber 42)) empty
      finalState <- execStmtTest (evalStmt (OutputStmt  testPos (EIdentifier  testPos "x"))) initialState
      stack initialState `shouldBe` stack finalState
      heap initialState `shouldBe` heap finalState

  describe "AssignmentStmt - identifier target" $ do
    it "assigns number to existing variable" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 42))) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 42)

    it "assigns expression result to variable" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt  testPos (EIdentifier  testPos "x") (BiOp  testPos Plus (Number  testPos 10) (Number  testPos 20)))) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 30)

    it "assigns variable to another variable" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 99)
            putsVar "y" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt  testPos (EIdentifier  testPos "y") (EIdentifier  testPos "x"))) state'
      let result = getVarFromState "y" finalState
      result `shouldBe` Just (VNumber 99)

    it "throws error for undefined variable" $ do
      execStmtTest (evalStmt (AssignmentStmt  testPos (EIdentifier  testPos "undefined") (Number  testPos 42))) empty
        `shouldThrow` anyException

  describe "AssignmentStmt - dereference target" $ do
    it "assigns through pointer" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 10)
            xAddr <- getsVarAddr "x"
            case xAddr of
              Just a -> putsVar "ptr" (Pointer a)
              Nothing -> Prelude.return ()) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt  testPos (UnOp  testPos Deref (EIdentifier  testPos "ptr")) (Number  testPos 42))) state'
      -- Check that dereferencing ptr now gives 42
      result <- evalStmtTest (evalExpr (UnOp  testPos Deref (EIdentifier  testPos "ptr"))) finalState
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
      finalState <- execStmtTest (evalStmt (AssignmentStmt  testPos (UnOp  testPos Deref (UnOp  testPos Deref (EIdentifier  testPos "ptr2"))) (Number  testPos 99))) state'
      result <- evalStmtTest (evalExpr (EIdentifier  testPos "x")) finalState
      result `shouldBe` VNumber 99

  describe "AssignmentStmt - field access target" $ do
    it "assigns to record field" $ do
      let state' = execStateTest (do
            xAddr <- putsValue (VNumber 10)
            yAddr <- putsValue (VNumber 20)
            putsVar "obj" (Interpreter.Data.Record [("x", xAddr), ("y", yAddr)])) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt  testPos (FieldAccess  testPos (EIdentifier  testPos "obj") "x") (Number  testPos 999))) state'
      result <- evalStmtTest (evalExpr (FieldAccess  testPos (EIdentifier  testPos "obj") "x")) finalState
      result `shouldBe` VNumber 999

    it "assigns to one field without affecting other fields" $ do
      let state' = execStateTest (do
            xAddr <- putsValue (VNumber 10)
            yAddr <- putsValue (VNumber 20)
            putsVar "obj" (Interpreter.Data.Record [("x", xAddr), ("y", yAddr)])) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt  testPos (FieldAccess  testPos (EIdentifier  testPos "obj") "x") (Number  testPos 100))) state'
      resultX <- evalStmtTest (evalExpr (FieldAccess  testPos (EIdentifier  testPos "obj") "x")) finalState
      resultY <- evalStmtTest (evalExpr (FieldAccess  testPos (EIdentifier  testPos "obj") "y")) finalState
      resultX `shouldBe` VNumber 100
      resultY `shouldBe` VNumber 20

  describe "Block" $ do
    it "executes empty block" $ do
      finalState <- execStmtTest (evalStmt (Block  testPos [])) empty
      stack finalState `shouldBe` stack empty
      heap finalState `shouldBe` heap empty

    it "executes single statement in block" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (Block  testPos [AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 42)])) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 42)

    it "executes multiple statements in sequence" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 0)
            putsVar "y" (VNumber 0)) empty
      let block = Block  testPos [
            AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 10),
            AssignmentStmt  testPos (EIdentifier  testPos "y") (Number  testPos 20)
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
      let block = Block  testPos [
            AssignmentStmt  testPos (EIdentifier  testPos "x") (BiOp  testPos Plus (EIdentifier  testPos "x") (Number  testPos 1)),  -- x = x + 1
            AssignmentStmt  testPos (EIdentifier  testPos "y") (BiOp  testPos Mul (EIdentifier  testPos "x") (Number  testPos 2))    -- y = x * 2
            ]
      finalState <- execStmtTest (evalStmt block) state'
      let resultX = getVarFromState "x" finalState
      let resultY = getVarFromState "y" finalState
      resultX `shouldBe` Just (VNumber 6)
      resultY `shouldBe` Just (VNumber 12)

  describe "IfStmt - without else" $ do
    it "executes then branch when condition is true (non-zero)" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt  testPos (Number  testPos 1) (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 42)) Nothing
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 42)

    it "skips then branch when condition is false (zero)" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt  testPos (Number  testPos 0) (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 42)) Nothing
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 0)

    it "evaluates condition expression correctly" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt  testPos (BiOp  testPos Gt (Number  testPos 5) (Number  testPos 3)) (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 99)) Nothing
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 99)

  describe "IfStmt - with else" $ do
    it "executes then branch when condition is true" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt  testPos (Number  testPos 1)
                     (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 10))
                     (Just (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 20)))
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 10)

    it "executes else branch when condition is false" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt  testPos (Number  testPos 0)
                     (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 10))
                     (Just (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 20)))
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 20)

    it "handles pointer as truthy condition" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt  testPos (UnOp  testPos Alloc (Number  testPos 42))
                     (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 1))
                     (Just (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 0)))
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 1)

    it "handles null pointer as falsy condition" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let ifStmt = IfStmt  testPos (Null  testPos)
                     (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 1))
                     (Just (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 2)))
      finalState <- execStmtTest (evalStmt ifStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 2)

  describe "WhileStmt" $ do
    it "does not execute when condition is initially false" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let whileStmt = WhileStmt  testPos (Number  testPos 0) (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 42))
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 0)

    it "executes body multiple times while condition is true" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 0)
            putsVar "i" (VNumber 0)) empty
      -- while (i < 3) { i = i + 1; }
      let whileStmt = WhileStmt  testPos
            (BiOp  testPos Gt (Number  testPos 3) (EIdentifier  testPos "i"))
            (AssignmentStmt  testPos (EIdentifier  testPos "i") (BiOp  testPos Plus (EIdentifier  testPos "i") (Number  testPos 1)))
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "i" finalState
      result `shouldBe` Just (VNumber 3)

    it "implements countdown loop" $ do
      let state' = execStateTest (putsVar "n" (VNumber 5)) empty
      -- while (n > 0) { n = n - 1; }
      let whileStmt = WhileStmt  testPos
            (BiOp  testPos Gt (EIdentifier  testPos "n") (Number  testPos 0))
            (AssignmentStmt  testPos (EIdentifier  testPos "n") (BiOp  testPos Minus (EIdentifier  testPos "n") (Number  testPos 1)))
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "n" finalState
      result `shouldBe` Just (VNumber 0)

    it "executes complex body with multiple statements" $ do
      let state' = execStateTest (do
            putsVar "i" (VNumber 0)
            putsVar "sum" (VNumber 0)) empty
      -- while (i < 5) { sum = sum + i; i = i + 1; }
      let whileStmt = WhileStmt  testPos
            (BiOp  testPos Gt (Number  testPos 5) (EIdentifier  testPos "i"))
            (Block  testPos [
              AssignmentStmt  testPos (EIdentifier  testPos "sum") (BiOp  testPos Plus (EIdentifier  testPos "sum") (EIdentifier  testPos "i")),
              AssignmentStmt  testPos (EIdentifier  testPos "i") (BiOp  testPos Plus (EIdentifier  testPos "i") (Number  testPos 1))
            ])
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let resultSum = getVarFromState "sum" finalState
      let resultI = getVarFromState "i" finalState
      resultSum `shouldBe` Just (VNumber 10)  -- 0+1+2+3+4 = 10
      resultI `shouldBe` Just (VNumber 5)

  describe "Complex statement combinations" $ do
    it "nested if statements" $ do
      let state' = execStateTest (putsVar "x" (VNumber 0)) empty
      let nestedIf = IfStmt  testPos (Number  testPos 1)
                       (IfStmt  testPos (Number  testPos 1)
                         (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 42))
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
      let whileStmt = WhileStmt  testPos
            (BiOp  testPos Gt (Number  testPos 5) (EIdentifier  testPos "i"))
            (Block  testPos [
              -- Skip modulo, just check if i is 0, 2, or 4 manually
              IfStmt  testPos (BiOp  testPos Eq (EIdentifier  testPos "i") (Number  testPos 0))
                (AssignmentStmt  testPos (EIdentifier  testPos "evens") (BiOp  testPos Plus (EIdentifier  testPos "evens") (Number  testPos 1)))
                Nothing,
              IfStmt  testPos (BiOp  testPos Eq (EIdentifier  testPos "i") (Number  testPos 2))
                (AssignmentStmt  testPos (EIdentifier  testPos "evens") (BiOp  testPos Plus (EIdentifier  testPos "evens") (Number  testPos 1)))
                Nothing,
              IfStmt  testPos (BiOp  testPos Eq (EIdentifier  testPos "i") (Number  testPos 4))
                (AssignmentStmt  testPos (EIdentifier  testPos "evens") (BiOp  testPos Plus (EIdentifier  testPos "evens") (Number  testPos 1)))
                Nothing,
              AssignmentStmt  testPos (EIdentifier  testPos "i") (BiOp  testPos Plus (EIdentifier  testPos "i") (Number  testPos 1))
            ])
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "evens" finalState
      result `shouldBe` Just (VNumber 3)

    it "if-else chain" $ do
      let state' = execStateTest (do
            putsVar "grade" (VNumber 85)
            putsVar "letter" (VNumber 0)) empty
      -- Simplified grade assignment
      let ifChain = IfStmt  testPos (BiOp  testPos Gt (EIdentifier  testPos "grade") (Number  testPos 90))
                      (AssignmentStmt  testPos (EIdentifier  testPos "letter") (Number  testPos 4))  -- A
                      (Just (IfStmt  testPos (BiOp  testPos Gt (EIdentifier  testPos "grade") (Number  testPos 80))
                        (AssignmentStmt  testPos (EIdentifier  testPos "letter") (Number  testPos 3))  -- B
                        (Just (AssignmentStmt  testPos (EIdentifier  testPos "letter") (Number  testPos 2)))))  -- C
      finalState <- execStmtTest (evalStmt ifChain) state'
      let result = getVarFromState "letter" finalState
      result `shouldBe` Just (VNumber 3)

    it "assignment with function call on right side" $ do
      let funDecl = FunDecl testPos "getVal" [] (FunBlock testPos [] [] (Number testPos 123))
      let state' = execStateTest (do
            putsVar "f" (Function funDecl)
            putsVar "x" (VNumber 0)) empty
      finalState <- execStmtTest (evalStmt (AssignmentStmt  testPos (EIdentifier  testPos "x") (Call  testPos (EIdentifier  testPos "f") []))) state'
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
      let whileStmt = WhileStmt  testPos
            (BiOp  testPos Gt (Number  testPos 3) (EIdentifier  testPos "i"))
            (Block  testPos [
              AssignmentStmt  testPos (UnOp  testPos Deref (EIdentifier  testPos "ptr"))
                (BiOp  testPos Plus (UnOp  testPos Deref (EIdentifier  testPos "ptr")) (Number  testPos 1)),
              AssignmentStmt  testPos (EIdentifier  testPos "i") (BiOp  testPos Plus (EIdentifier  testPos "i") (Number  testPos 1))
            ])
      finalState <- execStmtTest (evalStmt whileStmt) state'
      let result = getVarFromState "x" finalState
      result `shouldBe` Just (VNumber 3)
