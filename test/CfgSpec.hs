module CfgSpec (spec) where

import Test.Hspec
import Text.Parsec.Pos (SourcePos, newPos)
import Control.Monad.State (runState)
import qualified Data.Map as M

import Parse.AST
import Analysis.Cfg.Cfg
import Analysis.Cfg.Builder

-- Test position for AST nodes
testPos :: SourcePos
testPos = newPos "test" 0 0

-- Helper to build CFG from a single statement
buildCfgFromStmt :: Stmt SourcePos -> (CFGNode SourcePos, [CFGNode SourcePos], CFGMap SourcePos)
buildCfgFromStmt stmt = (root, ends, cfgMap)
  where
    (root, ends) = fst result
    cfgMap = snd result
    result = runState (buildStmt stmt) M.empty

-- Helper to get a node by ID from the map
getNode :: Int -> CFGMap a -> CFGNode a
getNode nodeId cfgMap = cfgMap M.! nodeId

spec :: Spec
spec = do
  describe "CFG Builder - Simple Statements" $ do
    it "builds CFG for a single output statement" $ do
      let stmt = OutputStmt testPos (Number testPos 42)
      let (root, ends, cfgMap) = buildCfgFromStmt stmt

      -- The root should be the only node
      getId root `shouldBe` 0

      -- Should have exactly one node in the map
      M.size cfgMap `shouldBe` 1

      -- The root should also be the only end node
      length ends `shouldBe` 1
      getId (head ends) `shouldBe` 0

      -- The node should have no predecessors or successors
      _prev root `shouldBe` []
      _next root `shouldBe` []

    it "builds CFG for a sequence of statements in a block" $ do
      let stmt1 = OutputStmt testPos (Number testPos 1)
      let stmt2 = OutputStmt testPos (Number testPos 2)
      let stmt3 = AssignmentStmt testPos (EIdentifier testPos "x") (Number testPos 3)
      let block = Block testPos [stmt1, stmt2, stmt3]
      let (root, ends, cfgMap) = buildCfgFromStmt block

      -- Should have 3 nodes (one for each statement)
      M.size cfgMap `shouldBe` 3

      -- Root should be the first statement
      getId root `shouldBe` 0

      -- Should have exactly one end node (the last statement)
      length ends `shouldBe` 1
      let endNode = head ends
      getId endNode `shouldBe` 2

      -- Check the chain: node 0 -> node 1 -> node 2
      _next root `shouldBe` [1]
      _prev root `shouldBe` []

      let middleNode = getNode 1 cfgMap
      _prev middleNode `shouldBe` [0]
      _next middleNode `shouldBe` [2]

      _prev endNode `shouldBe` [1]
      _next endNode `shouldBe` []

  describe "CFG Builder - Conditional Statements" $ do
    it "builds CFG for if statement without else" $ do
      let cond = BiOp testPos Eq (EIdentifier testPos "x") (Number testPos 0)
      let thenBody = OutputStmt testPos (Number testPos 42)
      let ifStmt = IfStmt testPos cond thenBody Nothing
      let (root, ends, cfgMap) = buildCfgFromStmt ifStmt

      -- Should have 2 nodes: if node and body node
      M.size cfgMap `shouldBe` 2

      -- Root is the if statement
      getId root `shouldBe` 0

      -- If has one successor (the then branch)
      _next root `shouldBe` [1]

      -- Should have 2 end nodes: the if node itself and the body node
      length ends `shouldBe` 2
      fmap getId ends `shouldSatisfy` \ids -> 0 `elem` ids && 1 `elem` ids

    it "builds CFG for if-else statement" $ do
      let cond = BiOp testPos Gt (EIdentifier testPos "x") (Number testPos 5)
      let thenBody = OutputStmt testPos (Number testPos 1)
      let elseBody = OutputStmt testPos (Number testPos 2)
      let ifStmt = IfStmt testPos cond thenBody (Just elseBody)
      let (root, ends, cfgMap) = buildCfgFromStmt ifStmt

      -- Should have 3 nodes: if node, then body, else body
      M.size cfgMap `shouldBe` 3

      -- Root is the if statement
      getId root `shouldBe` 0

      -- If has two successors (then and else branches)
      length (_next root) `shouldBe` 2
      _next root `shouldSatisfy` \successors -> 1 `elem` successors && 2 `elem` successors

      -- Should have 2 end nodes (both branches)
      length ends `shouldBe` 2

      -- Both branch nodes should point back to the if node as predecessor
      let node1 = getNode 1 cfgMap
      let node2 = getNode 2 cfgMap
      _prev node1 `shouldBe` [0]
      _prev node2 `shouldBe` [0]

  describe "CFG Builder - Loop Statements" $ do
    it "builds CFG for while loop" $ do
      let cond = BiOp testPos Gt (EIdentifier testPos "x") (Number testPos 0)
      let loopBody = AssignmentStmt testPos (EIdentifier testPos "x")
                                     (BiOp testPos Minus (EIdentifier testPos "x") (Number testPos 1))
      let whileStmt = WhileStmt testPos cond loopBody
      let (root, ends, cfgMap) = buildCfgFromStmt whileStmt

      -- Should have 2 nodes: while node and body node
      M.size cfgMap `shouldBe` 2

      -- Root is the while statement
      getId root `shouldBe` 0

      -- While points to body
      _next root `shouldBe` [1]

      -- Body should point back to while (creating a loop)
      let bodyNode = getNode 1 cfgMap
      _next bodyNode `shouldBe` [0]
      _prev bodyNode `shouldBe` [0]

      -- Only one end node: the while node itself
      length ends `shouldBe` 1
      getId (head ends) `shouldBe` 0
