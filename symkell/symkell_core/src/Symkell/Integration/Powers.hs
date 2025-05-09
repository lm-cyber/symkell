module Symkell.Integration.Powers (integrate) where

import Data.Text (Text)
import Symkell.Symbolic

integrate :: Text -> Expression -> Maybe Expression
integrate v (1 :/: Symbol s) =
  integrate v $ Symbol s :**: Number (-1)
integrate v (x :**: (Negate' (Number n :/: Number m))) =
  integrate v $ x :**: (Number (-n) :/: Number m)
integrate v (x :**: (Negate' (Number n))) =
  integrate v $ x :**: Number (-n)
integrate v e@(Number _) = Just $ e :*: Symbol v
integrate v e@(Symbol v')
  | v == v' = Just $ (Number 1 :/: Number 2) :*: (e :**: 2)
  | otherwise = Just $ e :*: Symbol v
integrate v (x@(Symbol s) :**: Number n)
  | s == v, -1 <- n = Just $ Log' x
  | s == v = Just $ (x :**: Number (n + 1)) :/: Number (n + 1)
  | otherwise = Nothing
integrate _ (_ :**: (_ :/: Number 0)) = Nothing
integrate v (x@(Symbol s) :**: y@(Number _ :/: Number _))
  | s == v = Just $ (x :**: (y :+: 1)) :/: (y :+: 1)
  | otherwise = Nothing
integrate _ _ = Nothing
