module StmtParserSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String (Parser)

import Parse.AST
import Parse.StmtParser
import qualified Lex.Lexer as Lexer

parseStmt :: String -> Either ParseError Stmt
parseStmt input = parse stmt "" input

spec :: Spec
spec = do
  describe "Output statement" $ do
    it "parses simple output statement" $
      parseStmt "output 42;" `shouldBe` Right (OutputStmt (Number 42))

    it "parses output with variable" $
      parseStmt "output x;" `shouldBe` Right (OutputStmt (EIdentifier "x"))

    it "parses output with expression" $
      parseStmt "output x + y;" `shouldBe` Right (OutputStmt (BiOp Plus (EIdentifier "x") (EIdentifier "y")))

  describe "Assignment statement" $ do
    it "parses simple assignment" $
      parseStmt "x = 42;" `shouldBe` Right (AssignmentStmt (EIdentifier "x") (Number 42))

    it "parses assignment with expression" $
      parseStmt "y = x + 1;" `shouldBe` Right (AssignmentStmt (EIdentifier "y") (BiOp Plus (EIdentifier "x") (Number 1)))

    it "parses field assignment" $
      parseStmt "obj.field = 10;" `shouldBe` Right (AssignmentStmt (FieldAccess (EIdentifier "obj") "field") (Number 10))

  describe "While statement" $ do
    it "parses while with simple condition and statement" $
      parseStmt "while (x > 0) output x;" `shouldBe` Right (WhileStmt (BiOp Gt (EIdentifier "x") (Number 0)) (OutputStmt (EIdentifier "x")))

    it "parses while with block" $
      parseStmt "while (true) { x = x + 1; }" `shouldBe` Right (WhileStmt (EIdentifier "true") (Block [AssignmentStmt (EIdentifier "x") (BiOp Plus (EIdentifier "x") (Number 1))]))

  describe "If statement" $ do
    it "parses if without else" $
      parseStmt "if (x == 0) output x;" `shouldBe` Right (IfStmt (BiOp Eq (EIdentifier "x") (Number 0)) (OutputStmt (EIdentifier "x")) Nothing)

    it "parses if with else" $
      parseStmt "if (x > 0) output x; else output 0;" `shouldBe` Right (IfStmt (BiOp Gt (EIdentifier "x") (Number 0)) (OutputStmt (EIdentifier "x")) (Just (OutputStmt (Number 0))))

    it "parses nested if statements" $
      parseStmt "if (x > 0) if (y > 0) output 1; else output 2;" `shouldBe` Right (IfStmt (BiOp Gt (EIdentifier "x") (Number 0)) (IfStmt (BiOp Gt (EIdentifier "y") (Number 0)) (OutputStmt (Number 1)) (Just (OutputStmt (Number 2)))) Nothing)

  describe "Block statement" $ do
    it "parses empty block" $
      parseStmt "{}" `shouldBe` Right (Block [])

    it "parses block with single statement" $
      parseStmt "{ output 42; }" `shouldBe` Right (Block [OutputStmt (Number 42)])

    it "parses block with multiple statements" $
      parseStmt "{ x = 1; y = 2; output x; }" `shouldBe` Right (Block [AssignmentStmt (EIdentifier "x") (Number 1), AssignmentStmt (EIdentifier "y") (Number 2), OutputStmt (EIdentifier "x")])

  describe "Complex statements" $ do
    it "parses while with complex block" $
      parseStmt "while (i > 10) { output i; i = i - 1; }" `shouldBe`
        Right (WhileStmt
          (BiOp Gt (EIdentifier "i") (Number 10))
          (Block [OutputStmt (EIdentifier "i"), AssignmentStmt (EIdentifier "i") (BiOp Minus (EIdentifier "i") (Number 1))]))

    it "parses if-else with blocks" $
      parseStmt "if (x > 0) { output 1; } else { output 0; }" `shouldBe`
        Right (IfStmt
          (BiOp Gt (EIdentifier "x") (Number 0))
          (Block [OutputStmt (Number 1)])
          (Just (Block [OutputStmt (Number 0)])))