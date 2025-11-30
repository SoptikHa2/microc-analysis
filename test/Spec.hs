import Test.Hspec

import ExprParserSpec
import DeclParserSpec
import StmtParserSpec
import StateSpec
import InterpretExprSpec
import InterpretStmtSpec
import ConstraintSolverSpec
import TypecheckSpec
import CfgSpec

main :: IO ()
main = hspec $ do
  describe "Expression Parser" ExprParserSpec.spec
  describe "Declaration Parser" DeclParserSpec.spec
  describe "Statement Parser" StmtParserSpec.spec
  describe "Interpreter State" StateSpec.spec
  describe "Expression Evaluation" InterpretExprSpec.spec
  describe "Statement Evaluation" InterpretStmtSpec.spec
  describe "Constraint Solver" ConstraintSolverSpec.spec
  describe "Type Checker (End-to-End)" TypecheckSpec.spec
  describe "CFG generator" CfgSpec.spec
