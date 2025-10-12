import Test.Hspec

import ExprParserSpec
import DeclParserSpec
import StmtParserSpec
import StateSpec
import InterpretExprSpec

main :: IO ()
main = hspec $ do
  describe "Expression Parser" ExprParserSpec.spec
  describe "Declaration Parser" DeclParserSpec.spec
  describe "Statement Parser" StmtParserSpec.spec
  describe "Interpreter State" StateSpec.spec
  describe "Expression Evaluation" InterpretExprSpec.spec