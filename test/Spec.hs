import Test.Hspec

import ExprParserSpec
import DeclParserSpec
import StmtParserSpec
import StateSpec

main :: IO ()
main = hspec $ do
  describe "Expression Parser" ExprParserSpec.spec
  describe "Declaration Parser" DeclParserSpec.spec
  describe "Statement Parser" StmtParserSpec.spec
  describe "Interpreter State" StateSpec.spec