module Data.Logic.Fml.Some.Some (
  -- * Testing Var Int
  atMostOneTest
--, fml2
--, fml3
--, fml4
--, fml5
--, fml6
--, fml7
--, fml8
--, fml9
--, fml10
--, fml11
--
--  -- * Teting Var String
--, fmlEvt1
--, fmlEvt2
) where
import qualified  Data.Logic.Var.Var    as Var
import qualified  Data.Logic.Fml.Fml    as Fml
import qualified Data.Logic.Fml.Combinator.Combinator as Combinator

  -- |Satisfiable empty CNF formula.
  --
  -- >>> fml1
  -- []
atMostOneTest :: String
atMostOneTest = Fml.prettyFormat (Combinator.get (Combinator.atMostOne [Var.mk i | i <- [1..4]]))

