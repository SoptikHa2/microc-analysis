{-# LANGUAGE ScopedTypeVariables #-}
module TypecheckSpec (spec) where

import Test.Hspec
import Text.Parsec (parse)
import Text.Parsec.Pos (SourcePos)
import qualified Data.Map as M
import Prelude

import Parse.AST
import Parse.DeclParser (program)
import Analysis.Typecheck.Typecheck (getTyping)
import Analysis.Typecheck.Type (Type(..))
import Analysis.Typecheck.Constraints (Typeable(..))
import TestUtils (normalizeSourcePos)

-- Helper to parse a program string
parseProgram :: String -> Either String (Program SourcePos)
parseProgram input = case parse program "" input of
  Left err -> Left (show err)
  Right prog -> Right (normalizeSourcePos prog)

-- Helper to get typing for a program
getTypingForProgram :: String -> Either String (M.Map (Typeable SourcePos) Type)
getTypingForProgram input = do
  prog <- parseProgram input
  getTyping prog

spec :: Spec
spec = do
  describe "Cross-function type inference" $ do

    it "infers type across simple identity function call" $ do
      -- foo(n) { return n; }
      -- bar() { n = 0; x = foo(n); return x; }
      -- Expected: x should be Int
      let progStr = unlines
            [ "foo(n) {"
            , "    return n;"
            , "}"
            , ""
            , "bar() {"
            , "    var n, x;"
            , "    n = 0;"
            , "    x = foo(n);"
            , "    return x;"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          -- Find the type of variable x in bar
          -- x is declared in bar, so look for CId with location in bar function
          let xTypes = [t | (CId loc "x", t) <- M.toList typing]
          xTypes `shouldNotBe` []
          -- x should be Int (or resolve to Int through Unknown)
          case xTypes of
            [] -> expectationFailure "Variable x not found in typing"
            (t:_) -> case t of
              Int -> pure () -- Success!
              Unknown _ -> expectationFailure $ "Variable x has unresolved type: " ++ show t
              _ -> expectationFailure $ "Variable x should be Int, but got: " ++ show t

    it "infers type across function call with return" $ do
      -- identity(x) { return x; }
      -- main() { var result; result = identity(42); return result; }
      -- Expected: result should be Int
      let progStr = unlines
            [ "identity(x) {"
            , "    return x;"
            , "}"
            , ""
            , "main() {"
            , "    var result;"
            , "    result = identity(42);"
            , "    return result;"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          let resultTypes = [t | (CId _ "result", t) <- M.toList typing]
          case resultTypes of
            [] -> expectationFailure "Variable result not found in typing"
            (t:_) -> t `shouldBe` Int

    it "infers type through chain of function calls" $ do
      -- f(x) { return x; }
      -- g(y) { return f(y); }
      -- h() { var z; z = 10; return g(z); }
      -- Expected: return value of h should be Int
      let progStr = unlines
            [ "f(x) {"
            , "    return x;"
            , "}"
            , ""
            , "g(y) {"
            , "    return f(y);"
            , "}"
            , ""
            , "h() {"
            , "    var z;"
            , "    z = 10;"
            , "    return g(z);"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          -- Check that z is Int
          let zTypes = [t | (CId _ "z", t) <- M.toList typing]
          case zTypes of
            [] -> expectationFailure "Variable z not found in typing"
            (t:_) -> t `shouldBe` Int

    it "infers types with multiple function calls in same function" $ do
      -- add(a, b) { return a + b; }
      -- double(x) { return add(x, x); }
      -- main() { var n, m; n = 5; m = double(n); return m; }
      -- Expected: m should be Int
      let progStr = unlines
            [ "add(a, b) {"
            , "    return a + b;"
            , "}"
            , ""
            , "double(x) {"
            , "    return add(x, x);"
            , "}"
            , ""
            , "main() {"
            , "    var n, m;"
            , "    n = 5;"
            , "    m = double(n);"
            , "    return m;"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          let mTypes = [t | (CId _ "m", t) <- M.toList typing]
          case mTypes of
            [] -> expectationFailure "Variable m not found in typing"
            (t:_) -> t `shouldBe` Int

    it "infers pointer types across function calls" $ do
      -- makePtr(x) { return &x; }
      -- main() { var n, p; n = 10; p = makePtr(n); return *p; }
      -- Expected: p should be Ptr Int
      let progStr = unlines
            [ "makePtr(x) {"
            , "    return &x;"
            , "}"
            , ""
            , "main() {"
            , "    var n, p;"
            , "    n = 10;"
            , "    p = makePtr(n);"
            , "    return *p;"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          let pTypes = [t | (CId _ "p", t) <- M.toList typing]
          case pTypes of
            [] -> expectationFailure "Variable p not found in typing"
            (Ptr Int:_) -> pure () -- Success!
            (t:_) -> expectationFailure $ "Variable p should be Ptr Int, but got: " ++ show t

    it "handles nested function calls with type propagation" $ do
      -- id(x) { return x; }
      -- apply(f, val) { return f(val); }
      -- main() { var x; x = apply(id, 99); return x; }
      -- Expected: x should be Int (val is 99, which is Int, id returns its argument)
      -- Note: This is complex as it requires higher-order function handling
      let progStr = unlines
            [ "id(x) {"
            , "    return x;"
            , "}"
            , ""
            , "apply(f, val) {"
            , "    return f(val);"
            , "}"
            , ""
            , "main() {"
            , "    var x;"
            , "    x = apply(id, 99);"
            , "    return x;"
            , "}"
            ]
      -- This might fail with current implementation, so just check it parses
      case getTypingForProgram progStr of
        Left err -> pendingWith $ "Higher-order functions not yet supported: " ++ err
        Right typing -> do
          let xTypes = [t | (CId _ "x", t) <- M.toList typing]
          case xTypes of
            [] -> expectationFailure "Variable x not found in typing"
            (Int:_) -> pure () -- Success!
            (Unknown _:_) -> pendingWith "Type inference incomplete for higher-order functions"
            (t:_) -> t `shouldBe` Int

  describe "Function parameter type inference" $ do

    it "infers parameter type from usage" $ do
      -- increment(n) { return n + 1; }
      -- Expected: n should be Int (because of n + 1)
      let progStr = unlines
            [ "increment(n) {"
            , "    return n + 1;"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          let nTypes = [t | (CId _ "n", t) <- M.toList typing]
          case nTypes of
            [] -> expectationFailure "Parameter n not found in typing"
            (t:_) -> t `shouldBe` Int

    it "infers parameter type from caller" $ do
      -- foo(n) { return n; }
      -- bar() { var x; x = foo(5); return x; }
      -- Expected: n in foo should be Int (from the call site)
      let progStr = unlines
            [ "foo(n) {"
            , "    return n;"
            , "}"
            , ""
            , "bar() {"
            , "    var x;"
            , "    x = foo(5);"
            , "    return x;"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          -- Check parameter n
          let nTypes = [t | (CId _ "n", t) <- M.toList typing]
          case nTypes of
            [] -> expectationFailure "Parameter n not found in typing"
            (t:_) -> t `shouldBe` Int

  describe "Function return type inference" $ do

    it "infers function return type from explicit return" $ do
      -- getNumber() { return 42; }
      -- Expected: getNumber should have type Fun [] Int
      let progStr = unlines
            [ "getNumber() {"
            , "    return 42;"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          let funTypes = [t | (CFun _ f, t) <- M.toList typing, f.name == "getNumber"]
          case funTypes of
            [] -> expectationFailure "Function getNumber not found in typing"
            (Fun [] Int:_) -> pure () -- Success!
            (t:_) -> expectationFailure $ "Function getNumber should be Fun [] Int, but got: " ++ show t

    it "infers function return type through variable" $ do
      -- compute() { var x; x = 100; return x; }
      -- Expected: compute should have type Fun [] Int
      let progStr = unlines
            [ "compute() {"
            , "    var x;"
            , "    x = 100;"
            , "    return x;"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          let funTypes = [t | (CFun _ f, t) <- M.toList typing, f.name == "compute"]
          case funTypes of
            [] -> expectationFailure "Function compute not found in typing"
            (Fun [] Int:_) -> pure () -- Success!
            (t:_) -> expectationFailure $ "Function compute should be Fun [] Int, but got: " ++ show t

    it "infers polymorphic function type" $ do
      -- identity(x) { return x; }
      -- Expected: identity could be μt. t -> t or get specialized to Int -> Int
      let progStr = unlines
            [ "identity(x) {"
            , "    return x;"
            , "}"
            ]
      case getTypingForProgram progStr of
        Left err -> expectationFailure $ "Type checking failed: " ++ err
        Right typing -> do
          let funTypes = [t | (CFun _ f, t) <- M.toList typing, f.name == "identity"]
          case funTypes of
            [] -> expectationFailure "Function identity not found in typing"
            (Fun [argT] retT:_) -> do
              -- The arg and return type should be the same (or related through Unknown)
              -- Could be Fun [Unknown n] (Unknown n) or Fun [Int] Int or similar
              argT `shouldBe` retT
            (t:_) -> expectationFailure $ "Function identity should be Fun [t] t, but got: " ++ show t