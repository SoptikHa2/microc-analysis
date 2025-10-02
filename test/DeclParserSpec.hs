module DeclParserSpec (spec) where

import Test.Hspec
import Text.Parsec
import Text.Parsec.String (Parser)

import Parse.AST
import Parse.DeclParser
import qualified Lex.Lexer as Lexer

parseProgram :: String -> Either ParseError Program
parseProgram input = parse program "" input

parseFunDecl :: String -> Either ParseError FunDecl
parseFunDecl input = parse func "" input

spec :: Spec
spec = do
  describe "Function declaration parsing" $ do
    it "parses function with no parameters and simple body" $
      parseFunDecl "main() { return 0; }" `shouldBe`
        Right (FunDecl "main" [] (FunBlock [] [] (Number 0)))

    it "parses function with one parameter" $
      parseFunDecl "double(x) { return x + x; }" `shouldBe`
        Right (FunDecl "double" ["x"] (FunBlock [] [] (BiOp Plus (EIdentifier "x") (EIdentifier "x"))))

    it "parses function with multiple parameters" $
      parseFunDecl "add(x, y) { return x + y; }" `shouldBe`
        Right (FunDecl "add" ["x", "y"] (FunBlock [] [] (BiOp Plus (EIdentifier "x") (EIdentifier "y"))))

    it "parses function with variable declarations" $
      parseFunDecl "test() { var x, y; return x; }" `shouldBe`
        Right (FunDecl "test" [] (FunBlock ["x", "y"] [] (EIdentifier "x")))

    it "parses function with statements" $
      parseFunDecl "test(x) { output x; return x; }" `shouldBe`
        Right (FunDecl "test" ["x"] (FunBlock [] [OutputStmt (EIdentifier "x")] (EIdentifier "x")))

    it "parses function with variables and statements" $
      parseFunDecl "complex(n) { var result; result = n * 2; output result; return result; }" `shouldBe`
        Right (FunDecl "complex" ["n"]
          (FunBlock ["result"]
            [AssignmentStmt (EIdentifier "result") (BiOp Mul (EIdentifier "n") (Number 2)),
             OutputStmt (EIdentifier "result")]
            (EIdentifier "result")))

    it "parses function with multiple variable declarations" $
      parseFunDecl "multiVar() { var x, y; var z; return x; }" `shouldBe`
        Right (FunDecl "multiVar" [] (FunBlock ["x", "y", "z"] [] (EIdentifier "x")))

  describe "Program parsing" $ do
    it "parses empty program" $
      parseProgram "" `shouldBe` Right []

    it "parses program with single function" $
      parseProgram "main() { return 0; }" `shouldBe`
        Right [FunDecl "main" [] (FunBlock [] [] (Number 0))]

    it "parses program with multiple functions" $
      parseProgram "add(x, y) { return x + y; } main() { return add(1, 2); }" `shouldBe`
        Right [
          FunDecl "add" ["x", "y"] (FunBlock [] [] (BiOp Plus (EIdentifier "x") (EIdentifier "y"))),
          FunDecl "main" [] (FunBlock [] [] (Call (EIdentifier "add") [Number 1, Number 2]))
        ]

  describe "Complex function examples" $ do
    it "parses factorial function" $
      parseFunDecl "factorial(n) { if (n == 0) return 1; else return n * factorial(n - 1); }" `shouldBe`
        Right (FunDecl "factorial" ["n"]
          (FunBlock []
            [IfStmt
              (BiOp Eq (EIdentifier "n") (Number 0))
              (Block [])
              (Just (Block []))]
            (BiOp Mul (EIdentifier "n") (Call (EIdentifier "factorial") [BiOp Minus (EIdentifier "n") (Number 1)]))))

    it "parses function with while loop" $
      parseFunDecl "sumTo(n) { var sum, i; sum = 0; i = 1; while (i <= n) { sum = sum + i; i = i + 1; } return sum; }" `shouldBe`
        Right (FunDecl "sumTo" ["n"]
          (FunBlock ["sum", "i"]
            [AssignmentStmt (EIdentifier "sum") (Number 0),
             AssignmentStmt (EIdentifier "i") (Number 1),
             WhileStmt (BiOp Gt (EIdentifier "i") (EIdentifier "n"))
               (Block [AssignmentStmt (EIdentifier "sum") (BiOp Plus (EIdentifier "sum") (EIdentifier "i")),
                       AssignmentStmt (EIdentifier "i") (BiOp Plus (EIdentifier "i") (Number 1))])]
            (EIdentifier "sum")))