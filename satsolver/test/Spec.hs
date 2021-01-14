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

--main = putStrLn (Fml.prettyFormat $ Combinator.get (Combinator.atMostOne [Var.mk i | i <- [1..4]]))

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [combinatorTests, fmlTests]

------------------------------ Fml tests ---------------------------------
fmlTests = testGroup "Fml tests"
  [ testCase "Test vars function" $
      assertEqual "Test vars function" [Var.mk "x1", Var.mk "x2"] $ Fml.vars (Fml.Or (Fml.Final (Var.mk "x1")) (Fml.Not (Fml.Final (Var.mk "x2"))))

  , testCase "Test depth function" $
      assertEqual "Test depth function" 2 $ Fml.depth (Fml.Or (Fml.Final (Var.mk "x1")) (Fml.Not (Fml.Final (Var.mk "x2"))))

  , testCase "Test toNNF function" $
      assertEqual "Test toNNF function" [Var.mk "x1"] $ Fml.vars (Fml.toNNF (Fml.Final (Var.mk "x1")))

  , testCase "Test toCNF function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test toDNF function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test isNNF function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test isCNF function" $
      assertEqual "Test isCCNF function" True $ Fml.isCNF(Fml.Or (Fml.Final (Var.mk "x1")) (Fml.Final (Var.mk "x2")))

  , testCase "Test isDNF function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test toUniversalNAnd function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test toUniversalNOr function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test isUniversalNAnd function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test isUniversalNOr function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test toCCNF function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test isCCNF function" $
      assertEqual "Test isCCNF function" True $ Fml.isCCNF(Fml.Or (Fml.Final (Var.mk "x1")) (Fml.Final (Var.mk "x2")))
  ]

-------------------------- Combinator tests ------------------------------
combinatorTests = testGroup "Combinator tests"
  [ testCase "Test multOr function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test multAnd function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test allOf function" $
      assertEqual "Test allOf function" "(1 . (2 . (3 . 4)))" (Fml.prettyFormat $ Combinator.get (Combinator.allOf [Var.mk i | i <- [1..4]]))

  , testCase "Test noneOf function" $
      assertEqual "Test noneOf function" "(-1 . (-2 . (-3 . -4)))" (Fml.prettyFormat $ Combinator.get (Combinator.noneOf [Var.mk i | i <- [1..4]]))

  , testCase "Test atLeast function" $
      assertEqual "Test atLeast function" "" (Fml.prettyFormat $ Combinator.get (Combinator.atLeast [Var.mk i | i <- [1..4]] 0))

  , testCase "Test atLeast function" $
      assertEqual "Test atLeast function" "(1 + (2 + (3 + 4)))" (Fml.prettyFormat $ Combinator.get (Combinator.atLeast [Var.mk i | i <- [1..4]] 1))

  , testCase "Test atLeast function" $
      assertEqual "Test atLeast function" "((1 . 2) + ((1 . 3) + ((1 . 4) + ((2 . 3) + ((2 . 4) + (3 . 4))))))" (Fml.prettyFormat $ Combinator.get (Combinator.atLeast [Var.mk i | i <- [1..4]] 2))

  , testCase "Test atLeastOne function" $
      assertEqual "Test atLeastOne function" "(1 + (2 + (3 + 4)))" (Fml.prettyFormat $ Combinator.get (Combinator.atLeastOne [Var.mk i | i <- [1..4]]))

  , testCase "Test atMost function" $
      assertEqual "Test atMost function" "" (Fml.prettyFormat $ Combinator.get (Combinator.atMost [Var.mk i | i <- [1..4]] 0))

  , testCase "Test atMost function" $
      assertEqual "Test atMost function" "((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) + ((-1 . (-3 . -4)) + (-2 . (-3 . -4)))))" (Fml.prettyFormat $ Combinator.get (Combinator.atMost [Var.mk i | i <- [1..4]] 1))

  , testCase "Test atMost function" $
      assertEqual "Test atMost function" "((-1 . -2) + ((-1 . -3) + ((-1 . -4) + ((-2 . -3) + ((-2 . -4) + (-3 . -4))))))" (Fml.prettyFormat $ Combinator.get (Combinator.atMost [Var.mk i | i <- [1..4]] 2))

  , testCase "Test atMostOne function" $
      assertEqual "Test atMostOne function" "((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) + ((-1 . (-3 . -4)) + (-2 . (-3 . -4)))))" (Fml.prettyFormat $ Combinator.get (Combinator.atMostOne [Var.mk i | i <- [1 .. 4]]))

  , testCase "Test exactly function" $
      assertEqual "Test exactly function" "" (Fml.prettyFormat $ Combinator.get (Combinator.exactly [Var.mk i | i <- [1..4]] 0))

  , testCase "Test exactly function" $
      assertEqual "Test exactly function" "((1 + (2 + (3 + 4))) . ((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) + ((-1 . (-3 . -4)) + (-2 . (-3 . -4))))))" (Fml.prettyFormat $ Combinator.get (Combinator.exactly [Var.mk i | i <- [1..4]] 1))

  , testCase "Test exactly function" $
      assertEqual "Test exactly function" "(((1 . 2) + ((1 . 3) + ((1 . 4) + ((2 . 3) + ((2 . 4) + (3 . 4)))))) .((-1 . -2) + ((-1 . -3) + ((-1 . -4) + ((-2 . -3) + ((-2 . -4) +(-3 . -4)))))))" (Fml.prettyFormat $ Combinator.get (Combinator.exactly [Var.mk i | i <- [1..4]] 2))

  , testCase "Test exactlyOne function" $
      assertEqual "Test exactlyOne function" "((1 + (2 + (3 + 4))) . ((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) +((-1 . (-3 . -4)) + (-2 . (-3 . -4))))))" (Fml.prettyFormat $ Combinator.get (Combinator.exactlyOne [Var.mk i | i <- [1..4]]))
  ]