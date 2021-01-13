-- Documentation stack test :
-- https://github.com/Originate/guide/blob/master/haskell/stack-tutorial.md
-- https://hackage.haskell.org/package/tasty-1.4.0.3/docs/Test-Tasty.html
module Spec where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Logic.Fml.Fml as Fml
import qualified Data.Logic.Fml.Combinator.Combinator as Combinator

main :: IO ()
main = do
  defaultMain (testGroup "Fml tests" [varsTest])--, depthTest, toNNFTest, toCNFTest, toDNFTest, isNNFTest, isCNFTest, isDNFTest, toUniversalNAndTest, toUniversalNOrTest, isUniversalNAndTest, isUniversalNOrTest, toCCNFTest, isCCNFTest])
--defaultMain (testGroup "Combinator tests" [multOrTest], multAndTest, allOfTest, noneOfTest, atLeastTest, atLeastOneTest, atMostTest, atMostOneTest, exactlyTest, exactlyOneTest])

------------------------------ Fml tests ---------------------------------

varsTest :: TestTree
varsTest = testCase "Testing vars function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))
{-
depthTest :: TestTree
depthTest = testCase "Testing depth function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

toNNFTest :: TestTree
toNNFTest = testCase "Testing toNNF function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

toCNFTest :: TestTree
toCNFTest = testCase "Testing toCNF function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

toDNFTest :: TestTree
toDNFTest = testCase "Testing toDNF function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

isNNFTest :: TestTree
isNNFTest = testCase "Testing isNNF function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

isCNFTest :: TestTree
isCNFTest = testCase "Testing isCNF function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

isDNFTest :: TestTree
isDNFTest = testCase "Testing isDNF function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

toUniversalNAndTest :: TestTree
toUniversalNAndTest = testCase "Testing toUniversalNAnd function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

toUniversalNOrTest :: TestTree
toUniversalNOrTest = testCase "Testing toUniversalNOr function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

isUniversalNAndTest :: TestTree
isUniversalNAndTest = testCase "Testing isUniversalNAnd function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

isUniversalNOrTest :: TestTree
isUniversalNOrTest = testCase "Testing isUniversalNOr function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

toCCNFTest :: TestTree
toCCNFTest = testCase "Testing toCCNF function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

isCCNFTest :: TestTree
isCCNFTest = testCase "Testing isCCNF function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

-------------------------- Combinator tests ------------------------------

multOrTest :: TestTree
multOrTest = testCase "Testing multOr function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

--multAndTest :: TestTree
--multAndTest = testCase "Testing multAnd function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

--allOfTest :: TestTree
--allOfTest = testCase "Testing allOf function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

--noneOfTest :: TestTree
--noneOfTest = testCase "Testing noneOf fuunction"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

--atLeastTest :: TestTree
--atLeastTest = testCase "Testing atLeast function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

--atLeastOneTest :: TestTree
--atLeastOneTest = testCase "Testing atLeastOne function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

--atMostTest :: TestTree
--atMostTest = testCase "Testing atMost function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

--atMostOneTest :: TestTree
--atMostOneTest = testCase "Testing atMostOne function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

--exactlyTest :: TestTree
--exactlyTest = testCase "Testing exactly function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))

--exactlyOneTest :: TestTree
--exactlyOneTest = testCase "Testing exactlyOne function"
  --(assertEqual "Should add 5 to get 10" 10 (multOr 5))
  -}