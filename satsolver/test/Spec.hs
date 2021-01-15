-- Documentation stack test :
-- https://github.com/Originate/guide/blob/master/haskell/stack-tutorial.md
-- https://hackage.haskell.org/package/tasty-1.4.0.3/docs/Test-Tasty.html
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Logic.Fml.Fml as Fml
import qualified Data.Logic.Fml.Combinator.Combinator as Combinator
import qualified Data.Logic.Var.Var as Var
import Data.Maybe
--import Data.List
--import Data.Ord

--main = putStrLn (Fml.prettyFormat $ Combinator.get (Combinator.atMostOne [Var.mk i | i <- [1..4]]))

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [fmlTests, combinatorTests]

------------------------------ Fml tests ---------------------------------
fmlTests = testGroup "Fml tests"
  [ testCase "Test vars function" $
      assertEqual "Test vars function" [Var.mk "x1", Var.mk "x2"] $ Fml.vars (Fml.Or (Fml.Final (Var.mk "x1")) (Fml.Not (Fml.Final (Var.mk "x2"))))

  , testCase "Test depth function" $
      assertEqual "Test depth function" 2 $ Fml.depth (Fml.Or (Fml.Final (Var.mk "x1")) (Fml.Not (Fml.Final (Var.mk "x2"))))

  , testCase "Test toUniversalNAnd function" $
      assertEqual "Test toUniversalNAnd function" "((((\"p\" ~. (\"q\" ~. \"q\")) ~. (\"p\" ~. (\"q\" ~. \"q\"))) ~. ((\"p\" ~. (\"q\" ~. \"q\")) ~. (\"p\" ~. (\"q\" ~. \"q\")))) ~. ((((\"p\" ~. \"p\") ~. \"q\") ~. ((\"p\" ~. \"p\") ~. \"q\")) ~. (((\"p\" ~. \"p\") ~. \"q\") ~. ((\"p\" ~. \"p\") ~. \"q\"))))" (Fml.prettyFormat $  Fml.toUniversalNAnd(Fml.XOr (Fml.Final (Var.mk "p")) (Fml.Final (Var.mk "q"))))

  , testCase "Test toCNF function" $
      assertEqual "Test toNNF function" [Var.mk "x1"] $ Fml.vars (Fml.toCNF (Fml.Final (Var.mk "x1")))

  , testCase "Test toDNF function" $
      [1, 2, 3] `compare` [1,2] @?= LT -- TODO

  , testCase "Test isNNF function with False Value" $
      assertEqual "Test isCCNF function" False $ Fml.isNNF(Fml.NAnd (Fml.Final (Var.mk "x1")) (Fml.Final (Var.mk "x2")))

  , testCase "Test isCNF function" $
      assertEqual "Test isCCNF function" True $ Fml.isCNF(Fml.Or (Fml.Final (Var.mk "x1")) (Fml.Final (Var.mk "x2")))

  , testCase "Test isDNF function" $
      assertEqual "Test isCCNF function" False $ Fml.isDNF(Fml.Not(Fml.And (Fml.Final (Var.mk "x1")) (Fml.Final (Var.mk "x2"))))

  , testCase "Test toUniversalNAnd function" $
     assertEqual "Test toUniversalNAnd function" "((((\"p\" ~. (\"q\" ~. \"q\")) ~. (\"p\" ~. (\"q\" ~. \"q\"))) ~. ((\"p\" ~. (\"q\" ~. \"q\")) ~. (\"p\" ~. (\"q\" ~. \"q\")))) ~. ((((\"p\" ~. \"p\") ~. \"q\") ~. ((\"p\" ~. \"p\") ~. \"q\")) ~. (((\"p\" ~. \"p\") ~. \"q\") ~. ((\"p\" ~. \"p\") ~. \"q\"))))" (Fml.prettyFormat $  Fml.toUniversalNAnd(Fml.XOr (Fml.Final (Var.mk "p")) (Fml.Final (Var.mk "q"))))

  , testCase "Test toUniversalNOr function" $
      assertEqual "Test toUniversalNOr function" "((((\"p\" ~+ \"p\") ~+ ((\"q\" ~+ \"q\") ~+ (\"q\" ~+ \"q\"))) ~+ (((\"p\" ~+ \"p\") ~+ (\"p\" ~+ \"p\")) ~+ (\"q\" ~+ \"q\"))) ~+ (((\"p\" ~+ \"p\") ~+ ((\"q\" ~+ \"q\") ~+ (\"q\" ~+ \"q\"))) ~+ (((\"p\" ~+ \"p\") ~+ (\"p\" ~+ \"p\")) ~+ (\"q\" ~+ \"q\"))))" (Fml.prettyFormat $ Fml.toUniversalNOr(Fml.XOr (Fml.Final (Var.mk "p")) (Fml.Final (Var.mk "q"))))

  , testCase "Test isUniversalNAnd function" $
      assertEqual "Test isUniversalNAnd function" True $ Fml.isUniversalNAnd(Fml.NAnd (Fml.Final (Var.mk "x1")) (Fml.Final (Var.mk "x2")))

  , testCase "Test isUniversalNOr function" $
       assertEqual "Test isUniversalNOr function" True $ Fml.isUniversalNOr(Fml.NOr (Fml.Final (Var.mk "x1")) (Fml.Final (Var.mk "x2")))

  , testCase "Test toCCNF function" $
       assertEqual "Test toUniversalNAnd function" "(A+B) . ( (B+C) . ( C +D ) . ( D+ E) ) )" (Fml.prettyFormat $  Fml.toCCNF(Fml.And ( Fml.And(Fml.Or(Fml.Final (Var.mk "A")) (Fml.Final (Var.mk "B")) ) (Fml.Or( Fml.Final (Var.mk "B")) (Fml.Final (Var.mk "C")))) ( Fml.And(Fml.Or(Fml.Final (Var.mk "C")) (Fml.Final (Var.mk "D")) ) (Fml.Or(Fml.Final (Var.mk "D")) (Fml.Final (Var.mk "E")))) ))


  , testCase "Test isCCNF function" $
      assertEqual "Test isCCNF function" True $ Fml.isCCNF(Fml.And (Fml.Final (Var.mk "x1")) (Fml.Or(Fml.Final (Var.mk "x2"))(Fml.Final (Var.mk "x3")) ))
  ]

-------------------------- Combinator tests ------------------------------
combinatorTests = testGroup "Combinator tests"
  [ testCase "Test multOr function" $
      assertEqual "Test multOr function" "(1 + (2 + (3 + 4)))" (Fml.prettyFormat $ Combinator.get (Combinator.multOr [ Fml.Final (Var.mk i) | i <- [1..4]]))

  , testCase "Test multAnd function" $
     assertEqual "Test multAnd function" "(1 . (2 . (3 . 4)))" (Fml.prettyFormat $ Combinator.get (Combinator.multAnd [ Fml.Final (Var.mk i) | i <- [1..4]]))

  , testCase "Test allOf function" $
      assertEqual "Test allOf function" "(1 . (2 . (3 . 4)))" (Fml.prettyFormat $ Combinator.get (Combinator.allOf [Var.mk i | i <- [1..4]]))

  , testCase "Test noneOf function" $
      assertEqual "Test noneOf function" "(-1 . (-2 . (-3 . -4)))" (Fml.prettyFormat $ Combinator.get (Combinator.noneOf [Var.mk i | i <- [1..4]]))

  , testCase "Test atLeast function with k= 0" $
      assertEqual "Test atLeast function with k = 0" True  ( isNothing (Combinator.atLeast [Var.mk i | i <- [1..4]] 0))

  , testCase "Test atLeast function with k = 1" $
      assertEqual "Test atLeast function with k = 1" "(1 + (2 + (3 + 4)))" (Fml.prettyFormat $ Combinator.get (Combinator.atLeast [Var.mk i | i <- [1..4]] 1))

  , testCase "Test atLeast function with k = 2" $
      assertEqual "Test atLeast function with k = 2" "((1 . 2) + ((1 . 3) + ((1 . 4) + ((2 . 3) + ((2 . 4) + (3 . 4))))))" (Fml.prettyFormat $ Combinator.get (Combinator.atLeast [Var.mk i | i <- [1..4]] 2))

  , testCase "Test atLeastOne function" $
      assertEqual "Test atLeastOne function" "(1 + (2 + (3 + 4)))" (Fml.prettyFormat $ Combinator.get (Combinator.atLeastOne [Var.mk i | i <- [1..4]]))

  , testCase "Test atMost function with k = 0" $
      assertEqual "Test atMost function with k = 0" True ( isNothing (Combinator.atMost [Var.mk i | i <- [1..4]] 0))

  , testCase "Test atMost function with k = 1" $
      assertEqual "Test atMost function with k = 1" "((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) + ((-1 . (-3 . -4)) + (-2 . (-3 . -4)))))" (Fml.prettyFormat $ Combinator.get (Combinator.atMost [Var.mk i | i <- [1..4]] 1))

  , testCase "Test atMost function with k = 2" $
      assertEqual "Test atMost function with k = 2" "((-1 . -2) + ((-1 . -3) + ((-1 . -4) + ((-2 . -3) + ((-2 . -4) + (-3 . -4))))))" (Fml.prettyFormat $ Combinator.get (Combinator.atMost [Var.mk i | i <- [1..4]] 2))

  , testCase "Test atMostOne function" $
      assertEqual "Test atMostOne function" "((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) + ((-1 . (-3 . -4)) + (-2 . (-3 . -4)))))" (Fml.prettyFormat $ Combinator.get (Combinator.atMostOne [Var.mk i | i <- [1 .. 4]]))

  , testCase "Test exactly function with k = 0" $
      assertEqual "Test exactly function with k = 0" True ( isNothing (Combinator.exactly [Var.mk i | i <- [1..4]] 0))

  , testCase "Test exactly function with k = 1" $
      assertEqual "Test exactly function with k = 1" "((1 + (2 + (3 + 4))) . ((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) + ((-1 . (-3 . -4)) + (-2 . (-3 . -4))))))" (Fml.prettyFormat $ Combinator.get (Combinator.exactly [Var.mk i | i <- [1..4]] 1))

  , testCase "Test exactly function with k = 2" $
      assertEqual "Test exactly function with k = 2" "(((1 . 2) + ((1 . 3) + ((1 . 4) + ((2 . 3) + ((2 . 4) + (3 . 4)))))) . ((-1 . -2) + ((-1 . -3) + ((-1 . -4) + ((-2 . -3) + ((-2 . -4) + (-3 . -4)))))))" (Fml.prettyFormat $ Combinator.get (Combinator.exactly [Var.mk i | i <- [1..4]] 2))

  , testCase "Test exactlyOne function" $
      assertEqual "Test exactlyOne function" "((1 + (2 + (3 + 4))) . ((-1 . (-2 . -3)) + ((-1 . (-2 . -4)) + ((-1 . (-3 . -4)) + (-2 . (-3 . -4))))))" (Fml.prettyFormat $ Combinator.get (Combinator.exactlyOne [Var.mk i | i <- [1..4]]))
  ]