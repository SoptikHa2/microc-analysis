module ExprParserSpec (spec) where

import Test.Hspec
import Text.Parsec

import Parse.AST
import Parse.ExprParser
import Text.Parsec.Pos (newPos)
import TestUtils (normalizeSourcePos)

testPos :: SourcePos
testPos = newPos "test" 0 0

parseExpr :: String -> Either ParseError (Expr SourcePos)
parseExpr input = fmap normalizeSourcePos (parse expression "" input)

spec :: Spec
spec = do
  describe "Number parsing" $ do
    it "parses positive integers" $
      parseExpr "42" `shouldBe` Right (Number  testPos 42)

    it "parses zero" $
      parseExpr "0" `shouldBe` Right (Number  testPos 0)

  describe "Identifier parsing" $ do
    it "parses simple identifiers" $
      parseExpr "x" `shouldBe` Right (EIdentifier  testPos "x")

    it "parses identifiers with underscores" $
      parseExpr "my_var" `shouldBe` Right (EIdentifier  testPos "my_var")

  describe "Binary operations" $ do
    it "parses addition" $
      parseExpr "1 + 2" `shouldBe` Right (BiOp  testPos Plus (Number  testPos 1) (Number  testPos 2))

    it "parses subtraction" $
      parseExpr "5 - 3" `shouldBe` Right (BiOp  testPos Minus (Number  testPos 5) (Number  testPos 3))

    it "parses multiplication" $
      parseExpr "4 * 6" `shouldBe` Right (BiOp  testPos Mul (Number  testPos 4) (Number  testPos 6))

    it "parses division" $
      parseExpr "8 / 2" `shouldBe` Right (BiOp  testPos Div (Number  testPos 8) (Number  testPos 2))

    it "parses equality" $
      parseExpr "x == y" `shouldBe` Right (BiOp  testPos Eq (EIdentifier  testPos "x") (EIdentifier  testPos "y"))

    it "parses greater than" $
      parseExpr "a > b" `shouldBe` Right (BiOp  testPos Gt (EIdentifier  testPos "a") (EIdentifier  testPos "b"))

  describe "Operator precedence" $ do
    it "multiplication has higher precedence than addition" $
      parseExpr "2 + 3 * 4" `shouldBe` Right (BiOp  testPos Plus (Number  testPos 2) (BiOp  testPos Mul (Number  testPos 3) (Number  testPos 4)))

    it "handles parentheses correctly" $
      parseExpr "(2 + 3) * 4" `shouldBe` Right (BiOp  testPos Mul (BiOp  testPos Plus (Number  testPos 2) (Number  testPos 3)) (Number  testPos 4))

  describe "Unary operations" $ do
    it "parses dereference" $
      parseExpr "*x" `shouldBe` Right (UnOp  testPos Deref (EIdentifier  testPos "x"))

    it "parses reference" $
      parseExpr "&y" `shouldBe` Right (UnOp  testPos Ref (EIdentifier  testPos "y"))

    it "parses alloc" $
      parseExpr "alloc z" `shouldBe` Right (UnOp  testPos Alloc (EIdentifier  testPos "z"))

  describe "Field access" $ do
    it "parses simple field access" $
      parseExpr "obj.field" `shouldBe` Right (FieldAccess  testPos (EIdentifier  testPos "obj") "field")

    it "parses chained field access" $
      parseExpr "obj.field1.field2" `shouldBe` Right (FieldAccess  testPos (FieldAccess  testPos (EIdentifier  testPos "obj") "field1") "field2")

  describe "Function calls" $ do
    it "parses function call with no arguments" $
      parseExpr "func()" `shouldBe` Right (Call { d =  testPos, target = EIdentifier  testPos "func", args = [] })

    it "parses function call with one argument" $
      parseExpr "func(42)" `shouldBe` Right (Call { d =  testPos, target = EIdentifier  testPos "func", args = [Number  testPos 42] })

    it "parses function call with multiple arguments" $
      parseExpr "func(1, x)" `shouldBe` Right (Call { d =  testPos, target = EIdentifier  testPos "func", args = [Number  testPos 1, EIdentifier  testPos "x"] })

  describe "Records" $ do
    it "parses empty record" $
      parseExpr "{}" `shouldBe` Right (Record  testPos $ Fields [] )

    it "parses record with one field" $
      parseExpr "{x: 42}" `shouldBe` Right (Record  testPos $ Fields [("x", Number  testPos 42)] )

    it "parses record with multiple fields" $
      parseExpr "{x: 1, y: 2}" `shouldBe` Right (Record  testPos $ Fields [("x", Number  testPos 1), ("y", Number  testPos 2)] )

  describe "Special expressions" $ do
    it "parses input" $
      parseExpr "input" `shouldBe` Right (Input  testPos)

    it "parses null" $
      parseExpr "null" `shouldBe` Right (Null  testPos)

  describe "Complex expressions" $ do
    it "parses complex arithmetic with function calls" $
      parseExpr "func(x) + 2 * y" `shouldBe` Right (BiOp  testPos Plus (Call { d =  testPos, target = EIdentifier  testPos "func", args = [EIdentifier  testPos "x"] }) (BiOp  testPos Mul (Number  testPos 2) (EIdentifier  testPos "y")))

    it "parses field access on function result" $
      parseExpr "func().field" `shouldBe` Right (FieldAccess  testPos (Call { d =  testPos, target = EIdentifier  testPos "func", args = [] }) "field")
    
  describe "Chained arithmetics" $ do
    it "parses double dereference (**x)" $
      parseExpr "**x" `shouldBe` Right (UnOp testPos Deref (UnOp testPos Deref (EIdentifier testPos "x")))
  
  describe "Comments" $ do
    it "near IDs" $
      parseExpr "/*foo*/x" `shouldBe` Right (EIdentifier testPos "x")
    it "whole line near Ids" $
      parseExpr "// foo \nx // this is x" `shouldBe` Right (EIdentifier testPos "x")