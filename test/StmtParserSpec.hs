module StmtParserSpec (spec) where

import Test.Hspec
import Text.Parsec

import Parse.AST
import Parse.StmtParser
import Text.Parsec.Pos (SourcePos, newPos)
import TestUtils (normalizeSourcePos)

testPos :: SourcePos
testPos = newPos "test" 0 0

parseStmt :: String -> Either ParseError (Stmt SourcePos)
parseStmt input = fmap normalizeSourcePos (parse stmt "" input)


spec :: Spec
spec = do
  describe "Output statement" $ do
    it "parses simple output statement" $
      parseStmt "output 42;" `shouldBe` Right (OutputStmt  testPos (Number  testPos 42))

    it "parses output with variable" $
      parseStmt "output x;" `shouldBe` Right (OutputStmt  testPos (EIdentifier  testPos "x"))

    it "parses output with expression" $
      parseStmt "output x + y;" `shouldBe` Right (OutputStmt  testPos (BiOp  testPos Plus (EIdentifier  testPos "x") (EIdentifier  testPos "y")))

  describe "Assignment statement" $ do
    it "parses simple assignment" $
      parseStmt "x = 42;" `shouldBe` Right (AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 42))

    it "parses assignment with expression" $
      parseStmt "y = x + 1;" `shouldBe` Right (AssignmentStmt  testPos (EIdentifier  testPos "y") (BiOp  testPos Plus (EIdentifier  testPos "x") (Number  testPos 1)))

    it "parses field assignment" $
      parseStmt "obj.field = 10;" `shouldBe` Right (AssignmentStmt  testPos (FieldAccess  testPos (EIdentifier  testPos "obj") "field") (Number  testPos 10))

  describe "While statement" $ do
    it "parses while with simple condition and statement" $
      parseStmt "while (x > 0) output x;" `shouldBe` Right (WhileStmt { d =  testPos, condition = BiOp  testPos Gt (EIdentifier  testPos "x") (Number  testPos 0), body = OutputStmt  testPos (EIdentifier  testPos "x") })

    it "parses while with block" $
      parseStmt "while (true) { x = x + 1; }" `shouldBe` Right (WhileStmt { d =  testPos, condition = EIdentifier  testPos "true", body = Block  testPos [AssignmentStmt  testPos (EIdentifier  testPos "x") (BiOp  testPos Plus (EIdentifier  testPos "x") (Number  testPos 1))] })

  describe "If statement" $ do
    it "parses if without else" $
      parseStmt "if (x == 0) output x;" `shouldBe` Right (IfStmt { d =  testPos, condition = BiOp  testPos Eq (EIdentifier  testPos "x") (Number  testPos 0), body = OutputStmt  testPos (EIdentifier  testPos "x"), elseBody = Nothing })

    it "parses if with else" $
      parseStmt "if (x > 0) output x; else output 0;" `shouldBe` Right (IfStmt { d =  testPos, condition = BiOp  testPos Gt (EIdentifier  testPos "x") (Number  testPos 0), body = OutputStmt  testPos (EIdentifier  testPos "x"), elseBody = Just (OutputStmt  testPos (Number  testPos 0)) })

    it "parses nested if statements" $
      parseStmt "if (x > 0) if (y > 0) output 1; else output 2;" `shouldBe` Right (IfStmt { d =  testPos, condition = BiOp  testPos Gt (EIdentifier  testPos "x") (Number  testPos 0), body = IfStmt { d =  testPos, condition = BiOp  testPos Gt (EIdentifier  testPos "y") (Number  testPos 0), body = OutputStmt  testPos (Number  testPos 1), elseBody = Just (OutputStmt  testPos (Number  testPos 2)) }, elseBody = Nothing })

  describe "Block statement" $ do
    it "parses empty block" $
      parseStmt "{}" `shouldBe` Right (Block  testPos [])

    it "parses block with single statement" $
      parseStmt "{ output 42; }" `shouldBe` Right (Block  testPos [OutputStmt  testPos (Number  testPos 42)])

    it "parses block with multiple statements" $
      parseStmt "{ x = 1; y = 2; output x; }" `shouldBe` Right (Block  testPos [AssignmentStmt  testPos (EIdentifier  testPos "x") (Number  testPos 1), AssignmentStmt  testPos (EIdentifier  testPos "y") (Number  testPos 2), OutputStmt  testPos (EIdentifier  testPos "x")])

  describe "Complex statements" $ do
    it "parses while with complex block" $
      parseStmt "while (i > 10) { output i; i = i - 1; }" `shouldBe`
        Right (WhileStmt
          { d =  testPos
          , condition = BiOp  testPos Gt (EIdentifier  testPos "i") (Number  testPos 10)
          , body = Block  testPos [OutputStmt  testPos (EIdentifier  testPos "i"), AssignmentStmt  testPos (EIdentifier  testPos "i") (BiOp  testPos Minus (EIdentifier  testPos "i") (Number  testPos 1))]
          })

    it "parses if-else with blocks" $
      parseStmt "if (x > 0) { output 1; } else { output 0; }" `shouldBe`
        Right (IfStmt
          { d =  testPos
          , condition = BiOp  testPos Gt (EIdentifier  testPos "x") (Number  testPos 0)
          , body = Block  testPos [OutputStmt  testPos (Number  testPos 1)]
          , elseBody = Just (Block  testPos [OutputStmt  testPos (Number  testPos 0)])
          })
  
  describe "Comments" $ do
    it "works in blocks" $
      parseStmt "{ /* inside */ }" `shouldBe` Right (Block testPos [])
    it "double comments" $
      parseStmt "{ /*in1*/ /*in2*/ }" `shouldBe` Right (Block testPos [])