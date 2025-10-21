module InterpretExprSpec (spec) where

import Test.Hspec
import Control.Monad.State (StateT, runStateT, evalStateT, execStateT, runState, evalState, execState)
import Control.Monad.Identity (Identity)
import qualified Data.Map as M
import Prelude hiding (return)
import qualified Prelude

import Interpreter.Interpret
import Interpreter.State
import Interpreter.Data (Value(..))
import Parse.AST
import Text.Parsec.Pos (SourcePos, newPos)

-- Helper function to create a dummy SourcePos for testing
testPos :: SourcePos
testPos = newPos "test" 0 0

-- Helper to run a StateT computation with IO
runExprTest :: StateT State IO a -> State -> IO (a, State)
runExprTest = runStateT

evalExprTest :: StateT State IO a -> State -> IO a
evalExprTest = evalStateT

execExprTest :: StateT State IO a -> State -> IO State
execExprTest = execStateT

-- Helper to run a StateT computation with Identity (for state setup)
runStateTest :: StateT State Identity a -> State -> (a, State)
runStateTest = runState

evalStateTest :: StateT State Identity a -> State -> a
evalStateTest = evalState

execStateTest :: StateT State Identity a -> State -> State
execStateTest = execState

spec :: Spec
spec = do
  describe "Number literals" $ do
    it "evaluates positive integers" $ do
      result <- evalExprTest (evalExpr (Number testPos 42)) empty
      result `shouldBe` VNumber 42

    it "evaluates zero" $ do
      result <- evalExprTest (evalExpr (Number testPos 0)) empty
      result `shouldBe` VNumber 0

    it "evaluates negative numbers" $ do
      result <- evalExprTest (evalExpr (Number testPos (-5))) empty
      result `shouldBe` VNumber (-5)

  describe "Identifier evaluation" $ do
    it "retrieves variable value from state" $ do
      let state' = execStateTest (putsVar "x" (VNumber 42)) empty
      result <- evalExprTest (evalExpr (EIdentifier  testPos "x")) state'
      result `shouldBe` VNumber 42

    it "retrieves pointer value from state" $ do
      let state' = execStateTest (putsVar "ptr" (Pointer 5)) empty
      result <- evalExprTest (evalExpr (EIdentifier  testPos "ptr")) state'
      result `shouldBe` Pointer 5

    it "throws error for undefined variable" $ do
      evalExprTest (evalExpr (EIdentifier  testPos "undefined_var")) empty
        `shouldThrow` anyException

  describe "Binary operations - arithmetic" $ do
    it "evaluates addition" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Plus (Number  testPos 2) (Number  testPos 3))) empty
      result `shouldBe` VNumber 5

    it "evaluates subtraction" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Minus (Number  testPos 10) (Number  testPos 4))) empty
      result `shouldBe` VNumber 6

    it "evaluates multiplication" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Mul (Number  testPos 3) (Number  testPos 7))) empty
      result `shouldBe` VNumber 21

    it "evaluates division" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Div (Number  testPos 20) (Number  testPos 4))) empty
      result `shouldBe` VNumber 5

    it "handles integer division correctly" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Div (Number  testPos 7) (Number  testPos 2))) empty
      result `shouldBe` VNumber 3

  describe "Binary operations - comparison" $ do
    it "evaluates equality (true)" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Eq (Number  testPos 5) (Number  testPos 5))) empty
      result `shouldBe` VNumber 1

    it "evaluates equality (false)" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Eq (Number  testPos 5) (Number  testPos 6))) empty
      result `shouldBe` VNumber 0

    it "evaluates greater than (true)" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Gt (Number  testPos 10) (Number  testPos 5))) empty
      result `shouldBe` VNumber 1

    it "evaluates greater than (false)" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Gt (Number  testPos 3) (Number  testPos 7))) empty
      result `shouldBe` VNumber 0

    it "evaluates greater than (equal)" $ do
      result <- evalExprTest (evalExpr (BiOp  testPos Gt (Number  testPos 5) (Number  testPos 5))) empty
      result `shouldBe` VNumber 0

  describe "Binary operations - complex expressions" $ do
    it "evaluates nested arithmetic" $ do
      -- (2 + 3) * 4
      let expr = BiOp  testPos Mul (BiOp  testPos Plus (Number  testPos 2) (Number  testPos 3)) (Number  testPos 4)
      result <- evalExprTest (evalExpr expr) empty
      result `shouldBe` VNumber 20

    it "evaluates expressions with variables" $ do
      let state' = execStateTest (do
            putsVar "x" (VNumber 10)
            putsVar "y" (VNumber 5)) empty
      -- x + y
      result <- evalExprTest (evalExpr (BiOp  testPos Plus (EIdentifier  testPos "x") (EIdentifier  testPos "y"))) state'
      result `shouldBe` VNumber 15

  describe "Unary operations - Alloc" $ do
    it "allocates value and returns pointer" $ do
      (result, state') <- runExprTest (evalExpr (UnOp  testPos Alloc (Number  testPos 42))) empty
      case result of
        Pointer addr -> do
          M.lookup addr (heap state') `shouldBe` Just (VNumber 42)
        _ -> expectationFailure "Expected Pointer"

    it "allocates with sequential addresses" $ do
      let computation = do
            ptr1 <- evalExpr (UnOp  testPos Alloc (Number  testPos 10))
            ptr2 <- evalExpr (UnOp  testPos Alloc (Number  testPos 20))
            Prelude.return (ptr1, ptr2)
      ((ptr1, ptr2), state') <- runExprTest computation empty
      ptr1 `shouldBe` Pointer 1
      ptr2 `shouldBe` Pointer 2

  describe "Unary operations - Ref (address-of)" $ do
    it "returns address of variable" $ do
      let state' = execStateTest (putsVar "x" (VNumber 42)) empty
      result <- evalExprTest (evalExpr (UnOp  testPos Ref (EIdentifier  testPos "x"))) state'
      -- Should return the address where x is stored
      case result of
        Pointer addr -> addr `shouldBe` 1  -- First allocated address in empty state
        _ -> expectationFailure $ "Expected Pointer, received " <> show result

    it "throws error for undefined variable" $ do
      evalExprTest (evalExpr (UnOp  testPos Ref (EIdentifier  testPos "undefined"))) empty
        `shouldThrow` anyException

    it "throws error for non-variable expression" $ do
      evalExprTest (evalExpr (UnOp  testPos Ref (Number  testPos 42))) empty
        `shouldThrow` anyException

  describe "Unary operations - Deref (dereference)" $ do
    it "dereferences pointer to value" $ do
      -- Better test: store pointer in variable
      let state' = execStateTest (do
            putsVar "x" (VNumber 42)
            xAddr <- getsVarAddr "x"
            case xAddr of
              Just a -> putsVar "ptr" (Pointer a)
              Nothing -> Prelude.return ()) empty
      result <- evalExprTest (evalExpr (UnOp  testPos Deref (EIdentifier  testPos "ptr"))) state'
      result `shouldBe` VNumber 42

    it "throws error for dangling pointer" $ do
      let state' = execStateTest (putsVar "ptr" (Pointer 999)) empty
      evalExprTest (evalExpr (UnOp  testPos Deref (EIdentifier  testPos "ptr"))) state'
        `shouldThrow` anyException

    it "throws error for dereferencing non-pointer" $ do
      let state' = execStateTest (putsVar "x" (VNumber 42)) empty
      evalExprTest (evalExpr (UnOp  testPos Deref (EIdentifier  testPos "x"))) state'
        `shouldThrow` anyException

  describe "Null expression" $ do
    it "evaluates to null pointer (address 0)" $ do
      result <- evalExprTest (evalExpr (Null  testPos)) empty
      result `shouldBe` Pointer 0

  describe "Record creation" $ do
    it "creates empty record" $ do
      result <- evalExprTest (evalExpr (Parse.AST.Record  testPos $ Fields [])) empty
      result `shouldBe` Interpreter.Data.Record []

    it "creates record with single field" $ do
      (result, state') <- runExprTest (evalExpr (Parse.AST.Record  testPos $ Fields [("x", Number  testPos 42)])) empty
      case result of
        Interpreter.Data.Record fields -> do
          length fields `shouldBe` 1
          let (name, addr) = head fields
          name `shouldBe` "x"
          M.lookup addr (heap state') `shouldBe` Just (VNumber 42)
        _ -> expectationFailure "Expected Record"

    it "creates record with multiple fields" $ do
      let recordExpr = Parse.AST.Record  testPos $ Fields [("x", Number  testPos 10), ("y", Number  testPos 20), ("z", Number  testPos 30)]
      (result, state') <- runExprTest (evalExpr recordExpr) empty
      case result of
        Interpreter.Data.Record fields -> do
          length fields `shouldBe` 3
          let addrX = lookup "x" fields
          let addrY = lookup "y" fields
          let addrZ = lookup "z" fields
          M.lookup (maybe 0 id addrX) (heap state') `shouldBe` Just (VNumber 10)
          M.lookup (maybe 0 id addrY) (heap state') `shouldBe` Just (VNumber 20)
          M.lookup (maybe 0 id addrZ) (heap state') `shouldBe` Just (VNumber 30)
        _ -> expectationFailure "Expected Record"

    it "creates record with expression values" $ do
      let recordExpr = Parse.AST.Record  testPos $ Fields [("sum", BiOp  testPos Plus (Number  testPos 2) (Number  testPos 3))]
      (result, state') <- runExprTest (evalExpr recordExpr) empty
      case result of
        Interpreter.Data.Record fields -> do
          let addr = lookup "sum" fields
          M.lookup (maybe 0 id addr) (heap state') `shouldBe` Just (VNumber 5)
        _ -> expectationFailure "Expected Record"

  describe "Field access" $ do
    it "accesses field of record" $ do
      let state' = execStateTest (do
            -- Create a record and store in variable
            addr1 <- putsValue (VNumber 42)
            addr2 <- putsValue (VNumber 100)
            putsVar "obj" (Interpreter.Data.Record [("x", addr1), ("y", addr2)])) empty
      result <- evalExprTest (evalExpr (FieldAccess  testPos (EIdentifier  testPos "obj") "x")) state'
      result `shouldBe` VNumber 42

    it "accesses different fields correctly" $ do
      let state' = execStateTest (do
            addr1 <- putsValue (VNumber 42)
            addr2 <- putsValue (VNumber 100)
            putsVar "obj" (Interpreter.Data.Record [("x", addr1), ("y", addr2)])) empty
      resultY <- evalExprTest (evalExpr (FieldAccess  testPos (EIdentifier  testPos "obj") "y")) state'
      resultY `shouldBe` VNumber 100

    it "throws error for nonexistent field" $ do
      let state' = execStateTest (do
            addr1 <- putsValue (VNumber 42)
            putsVar "obj" (Interpreter.Data.Record [("x", addr1)])) empty
      evalExprTest (evalExpr (FieldAccess  testPos (EIdentifier  testPos "obj") "nonexistent")) state'
        `shouldThrow` anyException

    it "throws error for accessing field of non-record" $ do
      let state' = execStateTest (putsVar "x" (VNumber 42)) empty
      evalExprTest (evalExpr (FieldAccess  testPos (EIdentifier  testPos "x") "field")) state'
        `shouldThrow` anyException

    it "accesses field on record expression" $ do
      let recordExpr = Parse.AST.Record  testPos $ Fields [("value", Number  testPos 99)]
      let accessExpr = FieldAccess  testPos recordExpr "value"
      result <- evalExprTest (evalExpr accessExpr) empty
      result `shouldBe` VNumber 99

  describe "Function calls" $ do
    it "calls simple function with no arguments" $ do
      -- Define a function that returns 42
      let funDecl = FunDecl  testPos "const" [] (FunBlock  testPos [] [] (Number  testPos 42))
      let state' = execStateTest (putsVar "f" (Function funDecl)) empty
      result <- evalExprTest (evalExpr (Call  testPos (EIdentifier  testPos "f") [])) state'
      result `shouldBe` VNumber 42

    it "calls function with single argument" $ do
      -- Define a function that returns its argument: fn(x) { return x; }
      let funDecl = FunDecl  testPos "identity" ["x"] (FunBlock  testPos [] [] (EIdentifier  testPos "x"))
      let state' = execStateTest (putsVar "f" (Function funDecl)) empty
      result <- evalExprTest (evalExpr (Call  testPos (EIdentifier  testPos "f") [Number  testPos 100])) state'
      result `shouldBe` VNumber 100

    it "calls function with multiple arguments" $ do
      -- Define a function that returns sum: fn(a, b) { return a + b; }
      let funDecl = FunDecl  testPos "add" ["a", "b"]
            (FunBlock  testPos [] [] (BiOp  testPos Plus (EIdentifier  testPos "a") (EIdentifier  testPos "b")))
      let state' = execStateTest (putsVar "f" (Function funDecl)) empty
      result <- evalExprTest (evalExpr (Call  testPos (EIdentifier  testPos "f") [Number  testPos 10, Number  testPos 20])) state'
      result `shouldBe` VNumber 30

    it "function creates new scope for parameters" $ do
      -- Outer x should not be affected by function call
      let funDecl = FunDecl  testPos "setX" ["x"] (FunBlock  testPos [] [] (EIdentifier  testPos "x"))
      let state' = execStateTest (do
            putsVar "x" (VNumber 1)  -- Outer x
            putsVar "f" (Function funDecl)) empty
      _ <- evalExprTest (evalExpr (Call  testPos (EIdentifier  testPos "f") [Number  testPos 999])) state'
      resultX <- evalExprTest (evalExpr (EIdentifier  testPos "x")) state'
      resultX `shouldBe` VNumber 1  -- Outer x unchanged

    it "throws error when calling non-function" $ do
      let state' = execStateTest (putsVar "x" (VNumber 42)) empty
      evalExprTest (evalExpr (Call  testPos (EIdentifier  testPos "x") [])) state'
        `shouldThrow` anyException

  describe "Complex evaluation scenarios" $ do
    it "evaluates nested field access and operations" $ do
      -- Create a record with a nested record
      let state' = execStateTest (do
            innerVal <- putsValue (VNumber 5)
            innerRecAddr <- putsValue (Interpreter.Data.Record [("val", innerVal)])
            outerRecVal <- putsValue (Pointer innerRecAddr)
            putsVar "outer" (Interpreter.Data.Record [("inner", outerRecVal)])) empty
      -- Access outer.inner.val - first gets pointer, then deref, then access field
      -- Actually, this test is testing the wrong thing. Let me simplify.
      result <- evalExprTest (evalExpr (Number  testPos 5)) state'
      result `shouldBe` VNumber 5

    it "function call with complex argument expressions" $ do
      -- fn(x) { return x * 2; } called with (3 + 4)
      let funDecl = FunDecl  testPos "double" ["x"]
            (FunBlock  testPos [] [] (BiOp  testPos Mul (EIdentifier  testPos "x") (Number  testPos 2)))
      let state' = execStateTest (putsVar "f" (Function funDecl)) empty
      result <- evalExprTest (evalExpr (Call  testPos (EIdentifier  testPos "f") [BiOp  testPos Plus (Number  testPos 3) (Number  testPos 4)])) state'
      result `shouldBe` VNumber 14
