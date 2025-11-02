{-# LANGUAGE ScopedTypeVariables #-}
module ConstraintSolverSpec (spec) where

import Test.Hspec
import qualified Data.Map as M

import Analysis.Typecheck.ConstraintSolver
import Analysis.Typecheck.Constraints
import Analysis.Typecheck.Type (Type(..), TypeError)

-- Type alias for test constraints with unit annotation
type TestConstraints = Constraints ()
type TestTypeable = Typeable ()

spec :: Spec
spec = do
  describe "solve - basic unification" $ do
    it "resolves a single Unknown to Int" $ do
      let constraints :: TestConstraints = [(CId "_" "x", Unknown 1), (CId "_" "x", Int)]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "x" :: TestTypeable, Int)])

    it "resolves Unknown through another Unknown" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "x", Unknown 1)
            , (CId "_" "y", Unknown 1)
            , (CId "_" "y", Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "x" :: TestTypeable, Int), (CId "_" "y", Int)])


    it "handles multiple independent Unknowns" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "x", Unknown 1)
            , (CId "_" "x", Int)
            , (CId "_" "y", Unknown 2)
            , (CId "_" "y", Ptr Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "x" :: TestTypeable, Int), (CId "_" "y", Ptr Int)])

    it "resolves chained Unknowns" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "a", Unknown 1)
            , (CId "_" "b", Unknown 1)
            , (CId "_" "c", Unknown 2)
            , (CId "_" "b", Unknown 2)
            , (CId "_" "c", Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "a" :: TestTypeable, Int), (CId "_" "b", Int), (CId "_" "c", Int)])

    it "handles already-resolved types" $ do
      let constraints :: TestConstraints = [(CId "_" "x", Int), (CId "_" "y", Ptr Int)]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "x" :: TestTypeable, Int), (CId "_" "y", Ptr Int)])

  describe "solve - pointer types" $ do
    it "resolves Unknown in pointer type" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "p", Ptr (Unknown 1))
            , (CId "_" "x", Unknown 1)
            , (CId "_" "x", Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "p" :: TestTypeable, Ptr Int), (CId "_" "x", Int)])

    it "resolves nested pointer types" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "pp", Ptr (Ptr (Unknown 1)))
            , (CId "_" "x", Unknown 1)
            , (CId "_" "x", Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "pp" :: TestTypeable, Ptr (Ptr Int)), (CId "_" "x", Int)])

    it "unifies pointer types" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "p", Ptr (Unknown 1))
            , (CId "_" "p", Ptr Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "p" :: TestTypeable, Ptr Int)])

  describe "solve - function types" $ do
    it "resolves Unknown in function return type" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "f", Fun [Int] (Unknown 1))
            , (CId "_" "x", Unknown 1)
            , (CId "_" "x", Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "f" :: TestTypeable, Fun [Int] Int), (CId "_" "x", Int)])

    it "resolves Unknown in function argument" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "f", Fun [Unknown 1] Int)
            , (CId "_" "x", Unknown 1)
            , (CId "_" "x", Ptr Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "f" :: TestTypeable, Fun [Ptr Int] Int), (CId "_" "x", Ptr Int)])

    it "resolves multiple Unknowns in function type" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "f", Fun [Unknown 1, Unknown 2] (Unknown 1))
            , (CId "_" "x", Unknown 1)
            , (CId "_" "x", Int)
            , (CId "_" "y", Unknown 2)
            , (CId "_" "y", Ptr Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList
        [ (CId "_" "f" :: TestTypeable, Fun [Int, Ptr Int] Int)
        , (CId "_" "x", Int)
        , (CId "_" "y", Ptr Int)
        ])

    it "unifies two function types" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "f", Fun [Unknown 1] Int)
            , (CId "_" "f", Fun [Ptr Int] Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "f" :: TestTypeable, Fun [Ptr Int] Int)])

  describe "solve - record types" $ do
    it "resolves Unknown in record field" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "r", Record [("x", Unknown 1), ("y", Int)])
            , (CId "_" "val", Unknown 1)
            , (CId "_" "val", Ptr Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList
        [ (CId "_" "r" :: TestTypeable, Record [("x", Ptr Int), ("y", Int)])
        , (CId "_" "val", Ptr Int)
        ])

    it "resolves multiple Unknowns in record" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "r", Record [("x", Unknown 1), ("y", Unknown 2)])
            , (CId "_" "a", Unknown 1)
            , (CId "_" "a", Int)
            , (CId "_" "b", Unknown 2)
            , (CId "_" "b", Ptr Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList
        [ (CId "_" "r" :: TestTypeable, Record [("x", Int), ("y", Ptr Int)])
        , (CId "_" "a", Int)
        , (CId "_" "b", Ptr Int)
        ])

    it "unifies two record types" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "r", Record [("x", Unknown 1), ("y", Int)])
            , (CId "_" "r", Record [("x", Ptr Int), ("y", Int)])
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList [(CId "_" "r" :: TestTypeable, Record [("x", Ptr Int), ("y", Int)])])

  describe "solve - error cases" $ do
    it "fails when merging incompatible concrete types" $ do
      let constraints :: TestConstraints = [(CId "_" "x", Int), (CId "_" "x", Ptr Int)]
      let result = solve constraints
      result `shouldSatisfy` isLeft

    it "fails when merging function types with different arities" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "f", Fun [Int] Int)
            , (CId "_" "f", Fun [Int, Int] Int)
            ]
      let result = solve constraints
      result `shouldSatisfy` isLeft

    it "fails when merging records with different fields" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "r", Record [("x", Int)])
            , (CId "_" "r", Record [("y", Int)])
            ]
      let result = solve constraints
      result `shouldSatisfy` isLeft

    it "fails when merging incompatible pointer types" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "p", Ptr Int)
            , (CId "_" "p", Ptr (Ptr Int))
            ]
      let result = solve constraints
      result `shouldSatisfy` isLeft

  describe "solve - complex scenarios" $ do
    it "resolves complex nested types" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "f", Fun [Ptr (Unknown 1)] (Ptr (Unknown 2)))
            , (CId "_" "x", Unknown 1)
            , (CId "_" "y", Unknown 2)
            , (CId "_" "x", Record [("val", Int)])
            , (CId "_" "y", Unknown 1)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList
        [ (CId "_" "f" :: TestTypeable, Fun [Ptr (Record [("val", Int)])] (Ptr (Record [("val", Int)])))
        , (CId "_" "x", Record [("val", Int)])
        , (CId "_" "y", Record [("val", Int)])
        ])

    it "handles transitive constraints across multiple typeables" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "a", Unknown 1)
            , (CId "_" "b", Unknown 2)
            , (CId "_" "c", Unknown 3)
            , (CId "_" "a", Unknown 2)
            , (CId "_" "b", Unknown 3)
            , (CId "_" "c", Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList
        [ (CId "_" "a" :: TestTypeable, Int)
        , (CId "_" "b", Int)
        , (CId "_" "c", Int)
        ])

    it "resolves function returning pointer to function" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "f", Fun [Int] (Ptr (Fun [Unknown 1] Int)))
            , (CId "_" "x", Unknown 1)
            , (CId "_" "x", Ptr Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList
        [ (CId "_" "f" :: TestTypeable, Fun [Int] (Ptr (Fun [Ptr Int] Int)))
        , (CId "_" "x", Ptr Int)
        ])
      
    it "resolves pointers" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "px", Ptr (Unknown 7))
            , (CId "_" "x", Unknown 7)
            , (CId "_" "px", Ptr Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList
        [ (CId "_" "px" :: TestTypeable, Ptr Int)
        , (CId "_" "x", Int)
        ])
    
    it "resolves records" $ do
      let constraints :: TestConstraints =
            [ (CId "_" "rx", Record [("a", (Unknown 1))] )
            , (CId "_" "x", Unknown 1)
            , (CId "_" "x", Int)
            ]
      let result = solve constraints
      result `shouldBe` Right (M.fromList
        [ (CId "_" "rx" :: TestTypeable, Record [("a", Int)])
        , (CId "_" "x", Int)
        ])

  describe "solve - recursive types" $ do
    it "creates recursive type for self-referential pointer" $ do
      -- Simulates: node = alloc {next: null}; (*node).next = node;
      -- node will be: ↑{ [("next",↑μ9 . { [("next",↑μ9)] })] } or similar
      let constraints :: TestConstraints =
            [ (CId "_" "node", Ptr (Unknown 9))                    -- node : ↑?9
            , (CId "_" "node", Ptr (Record [("next", Ptr (Unknown 9))]))  -- node : ↑{ [("next",↑?9)] }
            ]
      let result = solve constraints
      -- The recursive type may be nested, just check it resolves successfully
      case result of
        Left err -> expectationFailure $ "Expected success but got error: " ++ err
        Right solution -> do
          let nodeType = M.lookup (CId "_" "node" :: TestTypeable) solution
          case nodeType of
            Nothing -> expectationFailure "node type not found in solution"
            Just t -> do
              -- Check that it's a pointer to a record with a "next" field
              case t of
                Ptr (Record fields) -> do
                  case lookup "next" fields of
                    Just (Ptr _) -> return () -- Success - has recursive structure
                    _ -> expectationFailure $ "Expected 'next' field with pointer, but got: " ++ show t
                Ptr (TypeVarBinding _ (Record fields)) -> do
                  case lookup "next" fields of
                    Just (Ptr _) -> return () -- Success - has recursive structure
                    _ -> expectationFailure $ "Expected 'next' field with pointer, but got: " ++ show t
                _ -> expectationFailure $ "Expected pointer to record, but got: " ++ show t

    it "creates recursive type with multiple fields" $ do
      -- Simulates: node = alloc {v: x, prev: list, next: null}; (*node).next = node;
      let constraints :: TestConstraints =
            [ (CId "_" "node", Ptr (Record [("v", Unknown 4), ("prev", Unknown 3), ("next", Ptr (Unknown 9))]))
            , (CId "_" "node", Ptr (Unknown 9))
            ]
      let result = solve constraints
      case result of
        Left err -> expectationFailure $ "Expected success but got error: " ++ err
        Right solution -> do
          let nodeType = M.lookup (CId "_" "node" :: TestTypeable) solution
          case nodeType of
            Nothing -> expectationFailure "node type not found in solution"
            Just t -> do
              -- Check that it has the expected fields with recursive structure
              case t of
                Ptr (Record fields) -> do
                  -- Check fields exist
                  case (lookup "v" fields, lookup "prev" fields, lookup "next" fields) of
                    (Just _, Just _, Just (Ptr _)) -> return () -- Success
                    _ -> expectationFailure $ "Expected fields v, prev, next with next as pointer, but got: " ++ show t
                Ptr (TypeVarBinding _ (Record fields)) -> do
                  case (lookup "v" fields, lookup "prev" fields, lookup "next" fields) of
                    (Just _, Just _, Just (Ptr _)) -> return () -- Success
                    _ -> expectationFailure $ "Expected fields v, prev, next with next as pointer, but got: " ++ show t
                _ -> expectationFailure $ "Expected pointer to record, but got: " ++ show t

    it "handles assignment to different pointer's field" $ do
      -- Simulates: list = ...; node = alloc {next: null}; (*list).next = node;
      -- Both list and node should share the same recursive structure
      let constraints :: TestConstraints =
            [ (CId "_" "list", Ptr (Unknown 9))
            , (CId "_" "node", Ptr (Unknown 9))
            , (CId "_" "list", Ptr (Record [("next", Ptr (Unknown 9))]))
            , (CId "_" "node", Ptr (Record [("next", Ptr (Unknown 9))]))
            ]
      let result = solve constraints
      case result of
        Left err -> expectationFailure $ "Expected success but got error: " ++ err
        Right solution -> do
          let listType = M.lookup (CId "_" "list" :: TestTypeable) solution
          let nodeType = M.lookup (CId "_" "node" :: TestTypeable) solution
          case (listType, nodeType) of
            (Just lt, Just nt) -> do
              -- Both should be the same recursive type
              lt `shouldBe` nt
              -- And it should have the recursive structure (pointer to record with next field)
              case lt of
                Ptr (Record fields) -> do
                  case lookup "next" fields of
                    Just (Ptr _) -> return () -- Success
                    _ -> expectationFailure $ "Expected next field with pointer, but got: " ++ show lt
                Ptr (TypeVarBinding _ (Record fields)) -> do
                  case lookup "next" fields of
                    Just (Ptr _) -> return () -- Success
                    _ -> expectationFailure $ "Expected next field with pointer, but got: " ++ show lt
                _ -> expectationFailure $ "Expected pointer to record, but got: " ++ show lt
            _ -> expectationFailure "list or node type not found in solution"

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
