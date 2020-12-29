module Data.Logic.Fml.Combinator.Combinator (
  -- * type
--  Combinator(..)

  -- * constructing


  -- * testing


  -- * querying


  -- * Transforming

--, depth, vars
---- * Formatting
--, prettyFormat
---- * Transforming
--, toNNF, toCNF, toCCNF, toDNF, toUniversalNAnd
---- * Testing
--, isNNF, isCNF, isCCNF, isDNF
) where
--
--prettyFormat :: (Show a) => Fml a -> String
--prettyFormat (And   p q) = "(" ++ prettyFormat p ++ " . "   ++ prettyFormat q ++ ")"
--prettyFormat (NAnd  p q) = "(" ++ prettyFormat p ++ " ~. "  ++ prettyFormat q ++ ")"
--prettyFormat (Or    p q) = "(" ++ prettyFormat p ++ " + "   ++ prettyFormat q ++ ")"
--prettyFormat (NOr   p q) = "(" ++ prettyFormat p ++ " ~+ "  ++ prettyFormat q ++ ")"
--prettyFormat (XOr   p q) = "(" ++ prettyFormat p ++ " x+ "  ++ prettyFormat q ++ ")"
--prettyFormat (XNOr  p q) = "(" ++ prettyFormat p ++ " x~+ " ++ prettyFormat q ++ ")"
--prettyFormat (Imply p q) = "(" ++ prettyFormat p ++ " => "  ++ prettyFormat q ++ ")"
--prettyFormat (Equiv p q) = "(" ++ prettyFormat p ++ " <=> " ++ prettyFormat q ++ ")"
--prettyFormat (Not   p)   = "-" ++ prettyFormat p
--prettyFormat (Final v)   = show v