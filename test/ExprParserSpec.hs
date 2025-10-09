module ExprParserSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String (Parser)

import Parse.AST
import Parse.ExprParser
import qualified Lex.Lexer as Lexer

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse expression "" input

spec :: Spec
spec = do
  describe "Number parsing" $ do
    it "parses positive integers" $
      parseExpr "42" `shouldBe` Right (Number 42)

    it "parses zero" $
      parseExpr "0" `shouldBe` Right (Number 0)

  describe "Identifier parsing" $ do
    it "parses simple identifiers" $
      parseExpr "x" `shouldBe` Right (EIdentifier "x")

    it "parses identifiers with underscores" $
      parseExpr "my_var" `shouldBe` Right (EIdentifier "my_var")

  describe "Binary operations" $ do
    it "parses addition" $
      parseExpr "1 + 2" `shouldBe` Right (BiOp Plus (Number 1) (Number 2))

    it "parses subtraction" $
      parseExpr "5 - 3" `shouldBe` Right (BiOp Minus (Number 5) (Number 3))

    it "parses multiplication" $
      parseExpr "4 * 6" `shouldBe` Right (BiOp Mul (Number 4) (Number 6))

    it "parses division" $
      parseExpr "8 / 2" `shouldBe` Right (BiOp Div (Number 8) (Number 2))

    it "parses equality" $
      parseExpr "x == y" `shouldBe` Right (BiOp Eq (EIdentifier "x") (EIdentifier "y"))

    it "parses greater than" $
      parseExpr "a > b" `shouldBe` Right (BiOp Gt (EIdentifier "a") (EIdentifier "b"))

  describe "Operator precedence" $ do
    it "multiplication has higher precedence than addition" $
      parseExpr "2 + 3 * 4" `shouldBe` Right (BiOp Plus (Number 2) (BiOp Mul (Number 3) (Number 4)))

    it "handles parentheses correctly" $
      parseExpr "(2 + 3) * 4" `shouldBe` Right (BiOp Mul (BiOp Plus (Number 2) (Number 3)) (Number 4))

  describe "Unary operations" $ do
    it "parses dereference" $
      parseExpr "*x" `shouldBe` Right (UnOp Deref (EIdentifier "x"))

    it "parses reference" $
      parseExpr "&y" `shouldBe` Right (UnOp Ref (EIdentifier "y"))

    it "parses alloc" $
      parseExpr "alloc z" `shouldBe` Right (UnOp Alloc (EIdentifier "z"))

  describe "Field access" $ do
    it "parses simple field access" $
      parseExpr "obj.field" `shouldBe` Right (FieldAccess (EIdentifier "obj") "field")

    it "parses chained field access" $
      parseExpr "obj.field1.field2" `shouldBe` Right (FieldAccess (FieldAccess (EIdentifier "obj") "field1") "field2")

  describe "Function calls" $ do
    it "parses function call with no arguments" $
      parseExpr "func()" `shouldBe` Right (Call (EIdentifier "func") [])

    it "parses function call with one argument" $
      parseExpr "func(42)" `shouldBe` Right (Call (EIdentifier "func") [Number 42])

    it "parses function call with multiple arguments" $
      parseExpr "func(1, x)" `shouldBe` Right (Call (EIdentifier "func") [Number 1, EIdentifier "x"])

  describe "Records" $ do
    it "parses empty record" $
      parseExpr "{}" `shouldBe` Right (Record $ Fields [] )

    it "parses record with one field" $
      parseExpr "{x: 42}" `shouldBe` Right (Record $ Fields [("x", Number 42)] )

    it "parses record with multiple fields" $
      parseExpr "{x: 1, y: 2}" `shouldBe` Right (Record $ Fields [("x", Number 1), ("y", Number 2)] )

  describe "Special expressions" $ do
    it "parses input" $
      parseExpr "input" `shouldBe` Right Input

    it "parses null" $
      parseExpr "null" `shouldBe` Right Null

  describe "Complex expressions" $ do
    it "parses complex arithmetic with function calls" $
      parseExpr "func(x) + 2 * y" `shouldBe` Right (BiOp Plus (Call (EIdentifier "func") [EIdentifier "x"]) (BiOp Mul (Number 2) (EIdentifier "y")))

    it "parses field access on function result" $
      parseExpr "func().field" `shouldBe` Right (FieldAccess (Call (EIdentifier "func") []) "field")