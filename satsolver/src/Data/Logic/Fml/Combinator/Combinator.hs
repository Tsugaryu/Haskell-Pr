module Data.Logic.Fml.Combinator.Combinator
  ( -- * type

    --  Combinator(..)

    -- * constructing
    multOr,
    multAnd,
    allOf,
    noneOf,

    -- * testing

    -- * querying

    -- * Multiplication Function

    -- * Filter function

    --,exactlyOne
    --,exactly
    --,atMostOne
    atLeast,
    --,atLeastOne
    --,atMost
  )
where

import qualified Data.Logic.Fml.Fml as Fml
import qualified Data.Logic.Var.Var as Var
import qualified Data.Maybe as Maybe

--import qualified  .Prelude          as Pre

-- | ’multOr’ @fs@ returns the disjunction of the formulas in @fs.
--  -- It returns @Nothing@ if @fs@ is the empty list. V
--  >>> multOr [Fml.Final (Var.mk i) | i <- [1..4]]
--  Just (Or (Final 1) (Or (Final 2) (Or (Final 3) (Final 4)))
multOrContent :: [Fml.Fml a] -> Fml.Fml a
multOrContent (elt : elements) = Fml.Or elt (multOrContent elements)

multOr :: [Fml.Fml a] -> Maybe (Fml.Fml a)
multOr [] = Nothing
multOr elements = Just (multOrContent elements)

--Just (Fml.Or (elt) ( Just  multOr elements  ))

-- | ’multAnd’ @fs@ returns the conjunction of the formulas in @fs.
--  It returns @Nothing@ if @fs@ is the empty list.
--  multAnd [Fml.Final (Var.mk i) | i <- [1..4]]
--  Just (And (Final 1) (And (Final 2) (And (Final 3) (Final 4)))
multAndContent :: [Fml.Fml a] -> Fml.Fml a
multAndContent (elt : elements) = Fml.And elt (multAndContent elements)

multAnd :: [Fml.Fml a] -> Maybe (Fml.Fml a)
multAnd [] = Nothing
multAnd elements = Just (multAndContent elements)

fromVarToFml :: [Var.Var a] -> [Fml.Fml a]
fromVarToFml (elt : elements) = Fml.Final elt : fromVarToFml elements

fromVarToNegFml :: [Var.Var a] -> [Fml.Fml a]
fromVarToNegFml (elt : elements) = Fml.Not (Fml.Final elt) : fromVarToNegFml elements

-- | ’allOf’ @vs@ returns a formula that is satisfiable iff all variables
--  in @vs@ are true. The function returns @Nothing@ if @vs@ is the empty list. |

--Toutes les variables sont True si on effectue l'operation suivantes A V Not A
--Correction allOf : But : on considère une suite de variable formé par des AND est tout le temps True cf affichage sujet
allOf :: [Var.Var a] -> Maybe (Fml.Fml a)
allOf [] = Nothing
allOf elements = multAnd (fromVarToFml elements)

-- | ’noneOf’ @vs@ returns a formula that is satisfiable if no variable
--  in @vs@ is true. The function returns @Nothing@ if @vs@ is the empty list.
noneOf :: [Var.Var a] -> Maybe (Fml.Fml a)
noneOf [] = Nothing
noneOf elements = multAnd (fromVarToNegFml elements)

-- | ’atLeast’ @vs@ @k@ returns a formula that is satisfied iff at least @k@
--  variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
--  empty list or @k@ is non-positive or @k@ is larger than the number of
--  variables in @vs@.
composeSuccessiveFml :: ([Var.Var a] -> [Fml.Fml a]) -> Var.Var a -> [Var.Var a] -> Int -> [Fml.Fml a]
composeSuccessiveFml func elt [] n = []
composeSuccessiveFml func elt (x : elements) n = multAndContent (func (elt : take (n - 1) (tail elements))) : composeSuccessiveFml func elt (drop (n - 1) elements) n

--composeSuccessiveFml elt (x:elements) n = [multAndContent(fromVarToFml ([elt] ++ take(n-1 (tail(elements)))) ) ] ++ composeSuccessiveFml elt (drop n-1 elements) n

createFmlList :: ([Var.Var a] -> [Fml.Fml a]) -> [Var.Var a] -> Int -> [Fml.Fml a]
createFmlList func (x : elements) n = composeSuccessiveFml func x elements n ++ createFmlList func elements n

--Qd la fonction sera finie et checké on proposera une amélioration en utilisant allOf
atLeast :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)
atLeast [] k = Nothing
--atLeast [] 0 = Nothing
atLeast elements k = multOr (createFmlList fromVarToFml elements k)

--atLeast (elt:elements) k =  Just( Fml.Or( Maybe.fromJust (allOf ( take k elements ))) (Maybe.fromJust (noneOf (drop k elements ))) )
--Fml.Or ( Just ( allOf ( take k elements ) ) )   (  Just noneOf (drop k elements )    )

--multAnd ([Fml.Or ( Fml.Final( elt ) ) ( Fml.Not ( Fml.Final ( elt ) ) ) ] ++  fromVarToFml elements)

-- | ’atLeastOne’ @vs@ returns a formula that is satisfiable iff at least one
--  variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
--  empty list
atLeastOne :: [Var.Var a] -> Maybe (Fml.Fml a)
atLeastOne [] = Nothing
atLeastOne elements = atLeast elements 1

-- | ’atMost’ @vs@ @k@ returns a formula that is satisfiable iff at most @k@
--  variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
--  empty list or @k@ is non-positive or @k@ is larger than the number of
--  variables in @vs@.
-- définir le cas ou n = 1
atMost :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)
atMost [] n = Nothing
atMost elements n = multOr (createFmlList fromVarToNegFml elements n)

-- | ’atMostOne’ @vs@ returns a formula that is satisfiable iff at most one
--  variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
--  empty list.
atMostOne :: [Var.Var a] -> Maybe (Fml.Fml a)
atMostOne [] = Nothing
atMostOne elements = atMost elements 1

-- | ’exactly’ @vs@ @k@ returns a formula that is satisfiable iff exactly @k@
--  variables in @vs@ are true. The function returns @Nothing@ if @vs@ is the
--  empty list or @k@ is non-positive or @k@ is larger than the number of
--  variables in @vs@
exactly :: [Var.Var a] -> Int -> Maybe (Fml.Fml a)
exactly [] n = Nothing
exactly elements n = Just (Fml.And (get (atLeast elements n)) (get (atMost elements n)))

--

-- | ’exactlyOne’ @vs@ returns a formula that is satisfiable iff exactly one
--   variable in @vs@ is true. The function returns @Nothing@ if @vs@ is the
--  empty list.
get :: Maybe (Fml.Fml a) -> Fml.Fml a
get a = case a of
  Just x -> x

exactlyOne :: [Var.Var a] -> Maybe (Fml.Fml a)
exactlyOne [] = Nothing
exactlyOne elements = Just (Fml.And (get (atLeastOne elements)) (get (atMostOne elements)))