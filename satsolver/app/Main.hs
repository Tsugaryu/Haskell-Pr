module Main where

import Lib
import qualified  Data.Logic.Var.Var    as Var
import qualified  Data.Logic.Fml.Fml    as Fml
import qualified Data.Logic.Fml.Combinator.Combinator as Combinator

main :: IO ()
main = do
       putStrLn (Fml.prettyFormat $ Combinator.get (Combinator.atMostOne [Var.mk i | i <- [1..4]]))
