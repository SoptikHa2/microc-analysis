module DeclParserSpec (spec) where

import Test.Hspec
import Text.Parsec

import Parse.AST
import Parse.DeclParser
import Text.Parsec.Pos (newPos)
import TestUtils (normalizeSourcePos)

testPos :: SourcePos
testPos = newPos "test" 0 0

parseProgram :: String -> Either ParseError (Program SourcePos)
parseProgram input = fmap normalizeSourcePos (parse program "" input)

parseFunDecl :: String -> Either ParseError (FunDecl SourcePos)
parseFunDecl input = fmap normalizeSourcePos (parse func "" input)

spec :: Spec
spec = do
  describe "Function declaration parsing" $ do
    it "parses function with no parameters and simple body" $
      parseFunDecl "main() { return 0; }" `shouldBe`
        Right (FunDecl  testPos "main" [] (FunBlock  testPos [] [] (Number  testPos 0)))

    it "parses function with one parameter" $
      parseFunDecl "double(x) { return x + x; }" `shouldBe`
        Right (FunDecl  testPos "double" ["x"] (FunBlock  testPos [] [] (BiOp  testPos Plus (EIdentifier  testPos "x") (EIdentifier  testPos "x"))))

    it "parses function with multiple parameters" $
      parseFunDecl "add(x, y) { return x + y; }" `shouldBe`
        Right (FunDecl  testPos "add" ["x", "y"] (FunBlock  testPos [] [] (BiOp  testPos Plus (EIdentifier  testPos "x") (EIdentifier  testPos "y"))))

    it "parses function with variable declarations" $
      parseFunDecl "test() { var x, y; return x; }" `shouldBe`
        Right (FunDecl  testPos "test" [] (FunBlock  testPos ["x", "y"] [] (EIdentifier  testPos "x")))

    it "parses function with statements" $
      parseFunDecl "test(x) { output x; return x; }" `shouldBe`
        Right (FunDecl  testPos "test" ["x"] (FunBlock  testPos [] [OutputStmt  testPos (EIdentifier  testPos "x")] (EIdentifier  testPos "x")))

    it "parses function with variables and statements" $
      parseFunDecl "complex(n) { var result; result = n * 2; output result; return result; }" `shouldBe`
        Right (FunDecl  testPos "complex" ["n"]
          (FunBlock  testPos ["result"]
            [AssignmentStmt  testPos (EIdentifier  testPos "result") (BiOp  testPos Mul (EIdentifier  testPos "n") (Number  testPos 2)),
             OutputStmt  testPos (EIdentifier  testPos "result")]
            (EIdentifier  testPos "result")))

    it "parses function with multiple variable declarations" $
      parseFunDecl "multiVar() { var x, y; var z; return x; }" `shouldBe`
        Right (FunDecl  testPos "multiVar" [] (FunBlock  testPos ["x", "y", "z"] [] (EIdentifier  testPos "x")))

  describe "Program parsing" $ do
    it "parses empty program" $
      parseProgram "" `shouldBe` Right []

    it "parses program with single function" $
      parseProgram "main() { return 0; }" `shouldBe`
        Right [FunDecl  testPos "main" [] (FunBlock  testPos [] [] (Number  testPos 0))]

    it "parses program with multiple functions" $
      parseProgram "add(x, y) { return x + y; } main() { return add(1, 2); }" `shouldBe`
        Right [
          FunDecl  testPos "add" ["x", "y"] (FunBlock  testPos [] [] (BiOp  testPos Plus (EIdentifier  testPos "x") (EIdentifier  testPos "y"))),
          FunDecl  testPos "main" [] (FunBlock  testPos [] [] (Call  testPos (EIdentifier  testPos "add") [Number  testPos 1, Number  testPos 2]))
        ]
