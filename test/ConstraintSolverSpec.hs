{-# LANGUAGE ScopedTypeVariables #-}
module ConstraintSolverSpec (spec) where

import Test.Hspec
import qualified Data.Map as M

import Analysis.Typecheck.ConstraintSolver
import Analysis.Typecheck.Constraints
import Analysis.Typecheck.Type

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

    it "fails when Unknowns cannot be resolved" $ do
      let constraints :: TestConstraints = [(CId "_" "x", Unknown 1)]
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

-- Helper function to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
