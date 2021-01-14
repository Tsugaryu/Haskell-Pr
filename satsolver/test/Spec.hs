-- Documentation stack test :
-- https://github.com/Originate/guide/blob/master/haskell/stack-tutorial.md
-- https://hackage.haskell.org/package/tasty-1.4.0.3/docs/Test-Tasty.html
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Logic.Fml.Fml as Fml
import qualified Data.Logic.Fml.Combinator.Combinator as Combinator
import qualified Data.Logic.Var.Var as Var

--import Data.List
--import Data.Ord

main = putStrLn (Fml.prettyFormat $ Combinator.get (Combinator.atMostOne [Var.mk i | i <- [1..4]]))

{-
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [combinatorTests, fmlTests]

combinatorTests = testGroup "Combinator tests"
  [ testCase "Test atMostOne" $
      (assertEqual "Test atMostOne" ("Just \"((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) + ((-1 . (-3 . -4)) +(-2 . (-3 . -4)))))\"") (Fml.prettyFormat $ Combinator.get (Combinator.atMostOne [Var.mk i | i <- [1..4]])))

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

fmlTests = testGroup "Fml tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT

------------------------------ Fml tests ---------------------------------
fmlTests = testGroup "Fml tests"
  [ testCase "Test vars function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test depth function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test toNNF function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test toCNF function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test toDNF function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test isNNF function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test isCNF function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test isDNF function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test toUniversalNAnd function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test toUniversalNOr function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test isUniversalNAnd function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test isUniversalNOr function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test toCCNF function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test isCCNF function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO
  ]

-------------------------- Combinator tests ------------------------------
combinatorTests = testGroup "Combinator tests"
  [ testCase "Test multOr function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test multAnd function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test allOf function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test noneOf function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test atLeast function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test atLeastOne function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test atMost function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test atMostOne function" $
      assertEqual "Test atMostOne function" "Just \"((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) + ((-1 . (-3 . -4)) +(-2 . (-3 . -4)))))\"" (Fml.prettyFormat $ Combinator.get (Combinator.atMostOne [Var.mk i | i <- [1 .. 4]]))

  , testCase "Test exactly function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  , testCase "Test exactlyOne function" $
      [1, 2, 3] `compare` [1,2] @?= GT -- TODO

  ]
  -}