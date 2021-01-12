module Data.Logic.Fml.Fml
  ( -- * type
    Fml (..),

    -- * constructing

    --, mk
    --, (/++/)

    -- * Formatting
    prettyFormat,

    -- * testing
    isNNF,
    isCNF,
    --, isCCNF
    isDNF,

    -- * querying
    depth,
    vars,

    -- * Transforming
    toNNF,
    toCNF,
    --, toCCNF
    toDNF,
    toUniversalNAnd,
    toUniversalNOr,
  )
where

import qualified Data.List as List
import qualified Data.Logic.Var.Var as Var

--  import qualified Data.Foldable                   as F

--  import qualified Data.Map.Strict                 as M
--  import           Data.Maybe
--  import qualified Data.Set                        as S
--  import qualified Data.Tuple                      as T
--
--  import qualified Data.Logic.Clause as Clause
--  import qualified Data.Logic.Lit    as Lit
--  import qualified Data.Logic.Utils  as Utils

data Fml a
  = And (Fml a) (Fml a)
  | NAnd (Fml a) (Fml a)
  | Or (Fml a) (Fml a)
  | NOr (Fml a) (Fml a)
  | XOr (Fml a) (Fml a)
  | XNOr (Fml a) (Fml a)
  | Imply (Fml a) (Fml a)
  | Equiv (Fml a) (Fml a)
  | Not (Fml a) -- représente le literal négatif Fml.Not (Fml.Final (Var.mk "x"))
  | Final (Var.Var a) -- représente le litteral positif Fml.Final (Var.mk "x")
  deriving (Show)

-- | 'Fml' type
-- newtype Fml a = Fml { getClauses :: [Clause.Clause a] }

-- | show instance
-- instance (Show a) => Show (Fml a) where
--  show fml = "[" ++ List.intercalate "," (List.map show cs) ++ "]"
--    where
--      cs = getClauses fml

-- | ’prettyFormat’ @p@ return a string representation of the formula @p@.
--  -- and :        .
--  -- nand:        ~.
--  -- or:          +
--  -- nor:         ~+
--  -- xor:         x+
--  -- xnor:        x~+
--  -- imply:       =>
--  -- equivalence: <=>
--  -- not:         -
prettyFormat :: (Show a) => Fml a -> String
prettyFormat (And p q) = "(" ++ prettyFormat p ++ " . " ++ prettyFormat q ++ ")"
prettyFormat (NAnd p q) = "(" ++ prettyFormat p ++ " ~. " ++ prettyFormat q ++ ")"
prettyFormat (Or p q) = "(" ++ prettyFormat p ++ " + " ++ prettyFormat q ++ ")"
prettyFormat (NOr p q) = "(" ++ prettyFormat p ++ " ~+ " ++ prettyFormat q ++ ")"
prettyFormat (XOr p q) = "(" ++ prettyFormat p ++ " x+ " ++ prettyFormat q ++ ")"
prettyFormat (XNOr p q) = "(" ++ prettyFormat p ++ " x~+ " ++ prettyFormat q ++ ")"
prettyFormat (Imply p q) = "(" ++ prettyFormat p ++ " => " ++ prettyFormat q ++ ")"
prettyFormat (Equiv p q) = "(" ++ prettyFormat p ++ " <=> " ++ prettyFormat q ++ ")"
prettyFormat (Not p) = "-" ++ prettyFormat p
prettyFormat (Final v) = show v

-- |’vars’ @p@ returns all variables that occur in formula @p@. Duplicate
-- --  occurrences are removed.
varsWrapper :: (Eq a) => Fml a -> [Var.Var a]
varsWrapper (And   p q) = (varsWrapper p ++ varsWrapper q)
varsWrapper (NAnd  p q) = (varsWrapper p ++ varsWrapper q)
varsWrapper (Or    p q) = (varsWrapper p ++ varsWrapper q)
varsWrapper (NOr   p q) = (varsWrapper p ++ varsWrapper q)
varsWrapper (XOr   p q) = (varsWrapper p ++ varsWrapper q)
varsWrapper (XNOr  p q) = (varsWrapper p ++ varsWrapper q)
varsWrapper (Imply p q) = (varsWrapper p ++ varsWrapper q)
varsWrapper (Equiv p q) = (varsWrapper p ++ varsWrapper q)
varsWrapper (Not   p)   = varsWrapper p
varsWrapper (Final v)   = [v]


vars :: (Eq a) => Fml a -> [Var.Var a]
vars = List.nub . varsWrapper

----  La profondeur d’une formule est définie comme le nombre maximal de fonctions
----  logiques menant à une variable.
----  Pour toute variable x, la profondeur de la formule ( x
----est égale à 0 et celle de la formule(¬x)est égale à1.
---- Pour toutes formulesPetQet tout connecteur logique◦,
---- la profondeur de(P◦Q)est égale à un plus lemaximum des profondeurs des formulesPetQ

depth :: (Num b, Ord b) => Fml a -> b
depth (And   p q) =  max (depth p)  (depth q ) + 1
depth (NAnd  p q) =  max (depth p)  (depth q )+ 1
depth (Or    p q) =  max (depth p)  (depth q )+ 1
depth (NOr   p q) =  max (depth p)  (depth q )+ 1
depth (XOr   p q) =  max (depth p)  (depth q )+ 1
depth (XNOr  p q) =  max (depth p)  (depth q )+ 1
depth (Imply p q) =  max (depth p)  (depth q )+ 1
depth (Equiv p q) =  max (depth p)  (depth q )+ 1
depth (Not   p)   = depth p + 1
depth (Final v)   = 0

----NNF
---- si l’opérateur de la négation (¬) est appliqué uniquement aux variables, et les seuls opérateurs booléens
---- autorisés sont la conjonction (∧) et la disjonction (∨)
---- éliminer les connecteurs ↑,↓,⊕,point dans un cercle,⇒ et ⇔. Pour cela il faudra utiliser les tables de vérité
---- utiliser les lois de De Morgan pour distribuer¬sur∧et∨4, et
---- supprimer les double négations (i.e.¬¬F⇔F).

convertConnector :: Fml a -> Fml a
convertConnector (NAnd p q) = Or (Not (convertConnector p)) (Not (convertConnector q)) --equivaut a (¬p) ∨ (¬q)
convertConnector (NOr p q) = And (Not (convertConnector p)) (Not (convertConnector q)) -- equivaut a (¬p) ∧ (¬q)
convertConnector (XOr p q) = Or (And (convertConnector p) (Not (convertConnector q))) (And (Not (convertConnector p)) (convertConnector q)) -- equivaut a (p ∧ ¬q) ∨ (¬p ∧ q).
convertConnector (XNOr p q) = Or (And (convertConnector p) (convertConnector q)) (And (Not (convertConnector p)) (Not (convertConnector q))) -- equivaut a (p n q) v ( ¬p n ¬q)
convertConnector (Imply p q) = Or (Not (convertConnector p)) (convertConnector q) -- equivaut à ¬p ∨ q
convertConnector (Equiv p q) = Or (And (convertConnector p) (convertConnector q)) (And (Not (convertConnector q)) (Not (convertConnector p))) -- equivaut a (p ∧ q) ∨ (¬q ∧ ¬p)
convertConnector (Not p) = p
convertConnector f@(Final v) = f

---- La relation de DeMorgan dit que ¬(pnq)<-> ¬p u ¬q et ¬puq <-> ¬p n ¬q
deMorgan :: Fml a -> Fml a
deMorgan (Not (And p q)) = Or (Not (deMorgan p)) (Not (deMorgan q))
deMorgan (Not (Or p q)) = And (Not (deMorgan p)) (Not (deMorgan q))
deMorgan (Or p q) = Or (deMorgan p) (deMorgan q)
deMorgan (And p q) = And (deMorgan p) (deMorgan q)
deMorgan (Not p) = Not (deMorgan p)
deMorgan a = a

delNegation :: Fml a -> Fml a
delNegation (Not (Not p)) = delNegation p
delNegation (Not p) = Not (delNegation p)
delNegation (And p q) = And (delNegation p) (delNegation q)
delNegation (Or p q) = Or (delNegation p) (delNegation q)

toNNF :: Fml a -> Fml a
toNNF = delNegation . deMorgan . convertConnector

--
----Forme normale conjonctive
----si elle se compose d’une conjonction de clause
---- une clause est une disjonction de littérau
----Process
----utiliser la fonctionToNNF
----redistribuer V sur N des  V a l interieurs et des N a l exterieurs
----A_(B^C)=(A_B)^(A_C)
----AnB OK
--


distribVToN :: Fml a -> Fml a
distribVToN (Or p (And q r)) = And (distribVToN (Or (distribVToN p) (distribVToN q))) (distribVToN (Or (distribVToN p) (distribVToN r)))
distribVToN (Or (And q r) p) = And (Or (distribVToN p) (distribVToN q)) (Or (distribVToN p) (distribVToN r))
distribVToN (And p q) = And (distribVToN p) (distribVToN q)
distribVToN (Or p q) = Or (distribVToN p) (distribVToN q)

--
----TODO : chercher un moyen pour qu'on puisse prendre soit la fonction distribVToN ou distribNToV
---- | Pour connaitre le nombre de Cycle, il est nécessaire d'appeler la fonction depth
--applyNTimes :: Int -> (Fml a -> Fml a) -> Fml  a
--applyNTimes n f val = foldl (\s e -> e s) val [f | x <- [1..n]]
toCNF :: Fml a -> Fml a
toCNF = distribVToN . toNNF

----DNF
----Process
----Transformer en une formule NNF
---- distribuer N sur V cad n'avoir que  des N a l interieur des parentheses et des V à l exterieur :
---- Pour AuB = Au(¬AnB) ;
---- x^(y_z)=(x^y)_(x^z)
----ptet necessaire de faire la transformation sur chaque coté
--
distribNtoV :: Fml a -> Fml a
distribNtoV (And p (Or q r)) = Or (distribNtoV (And (distribNtoV p) (distribNtoV q))) (distribNtoV (And (distribNtoV p) (distribNtoV r)))
distribNtoV (And (Or q r) p) = Or (And (distribNtoV p) (distribNtoV q)) (And (distribNtoV p) (distribNtoV r))
distribNtoV (And p q) = And (distribNtoV p) (distribNtoV q)
distribNtoV (Or p q) = Or (distribNtoV p) (distribNtoV q)

toDNF :: Fml a -> Fml a
toDNF = distribNtoV . toNNF

--
---- |’isNNF’ @f@ returns true iff formula @f@ is NNF.
isNNF :: Fml a -> Bool
isNNF (NAnd p q) = False
isNNF (NOr p q) = False
isNNF (XOr p q) = False
isNNF (XNOr p q) = False
isNNF (Imply p q) = False
isNNF (Equiv p q) = False
isNNF (Not (Final p)) = True
isNNF (Final p) = True
isNNF (Not p) = False
isNNF (And p q) = isNNF p && isNNF q
isNNF (Or p q) = isNNF p && isNNF q

--
---- |’isCNF’ @f@ returns true iff formula @f@ is CNF.
isCNF :: Fml a -> Bool
isCNF (Not (Or p q)) = False
isCNF (Not (And p q)) = False
isCNF (And p (Or q r)) = False
isCNF (And (Or q r) p) = False
isCNF (And p q) = isCNF p && isCNF q
isCNF (Or p q) = isCNF p && isCNF q
isCNF (Not p) = True
isCNF (Final v) = True

---- |’isDNF’ @f@ returns true iff formula @f@ is DNF.
isDNF :: Fml a -> Bool
isDNF (And p (Or q r)) = False
isDNF (And (Or q r) p) = False
isDNF (And p q) = isDNF p && isDNF q
isDNF (Or p q) = isDNF p && isDNF q
isDNF (Not (Or p q)) = False
isDNF (Not (And p q)) = False
isDNF (Not p) = True
isDNF (Final v) = True


---- |’toUniversalNAnd’ @p@ returns a NAND-formula that is equivalent-- to formula @p@.1
toNAND :: Fml a -> Fml a
toNAND (Not p) = NAnd (toNAND p) (toNAND p) --NAND = fleche du haut NOR fleche du bas
toNAND (And p q) = NAnd (NAnd (toNAND p) (toNAND q)) (NAnd (toNAND p) (toNAND q))
toNAND (Or p q) = NAnd (NAnd (toNAND p) (toNAND p)) (NAnd (toNAND q) (toNAND q))

--
toUniversalNAnd :: Fml a -> Fml a
toUniversalNAnd  = toNAND . toNNF

--
toNOR :: Fml a -> Fml a
toNOR (Not p) = NOr (toNOR p) (toNOR p) --NAND = fleche du haut NOR fleche du bas
toNOR (And p q) = NOr (NOr (toNOR p) (toNOR p)) (NOr (toNOR q) (toNOR q))
toNOR (Or p q) = NOr (NOr (toNOR p) (toNOR q)) (NOr (toNOR p) (toNOR q))

--
toUniversalNOr :: Fml a -> Fml a
toUniversalNOr = toNOR . toNNF

isUniversalNAnd :: Fml a -> Bool
isUniversalNAnd (NAnd p q) = isUniversalNAnd p && isUniversalNAnd q
isUniversalNAnd (Final p) = True
isUniversalNAnd (NOr p q) = False
isUniversalNAnd (XOr p q) = False
isUniversalNAnd (XNOr p q) = False
isUniversalNAnd (Imply p q) = False
isUniversalNAnd (Equiv p q) = False
isUniversalNAnd (Not p) = False

--
--
isUniversalNOr :: Fml a -> Bool
isUniversalNOr (NAnd p q) = False
isUniversalNOr (Final p) = True
isUniversalNOr (NOr p q) = isUniversalNOr p && isUniversalNOr q
isUniversalNOr (XOr p q) = False
isUniversalNOr (XNOr p q) = False
isUniversalNOr (Imply p q) = False
isUniversalNOr (Equiv p q) = False
isUniversalNOr (Not p) = False

translation :: Fml a -> Fml a -> Fml a -> Fml a
translation a b c = And a (And b c)

translateAlgo :: Fml a -> Fml a
translateAlgo fml@(And (And r s) q) = if isOneClause s then switchAlgo (translation r s q) else translateAlgo (translation r s q) -- déplace

switcher :: Fml a -> Fml a
switcher (And p q) = And q (switchAlgo p)

isOneClause :: Fml a -> Bool
isOneClause (And p q) = False
isOneClause (Or p q) = True

switchAlgo :: Fml a -> Fml a
switchAlgo fml@(And p q)
  | not (isOneClause p) && isOneClause q = switcher fml
  | not (isOneClause p) && not (isOneClause q) =
    translateAlgo fml
  | otherwise = switchAlgo q

toCCNF :: Fml a -> Fml a
toCCNF a = switchAlgo (toCNF a)

isCCNF :: Fml a -> Bool
isCCNF (And p q) = isOneClause p && isCCNF q

--{---Aller plus loin
--Une fonction de comparaison risque d'être utile pour les cas And/Or p p
--simplify :: Fml a -> Fml a
--simplify (And   p q) = And (simplify p)( simplify q)
--simplify (Or   p q) = Or (simplify p)  (simplify q)
--simplify (And p p )= p
--simplify (Or p p )= p
--simplify(Not (Not p)) = p-}
