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
  ]
  -}