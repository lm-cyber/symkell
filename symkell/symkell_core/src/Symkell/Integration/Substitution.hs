module Symkell.Integration.Substitution (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symkell.Differentiation
import Symkell.Integration.Factor
import Symkell.Symbolic

integrate ::
  [Text -> Expression -> Maybe Expression] ->
  Text ->
  Expression ->
  Maybe Expression
integrate fs v (x :*: UnaryApply func y)
  | Number 0 <- d = Nothing
  | x' == y',
    Just e <- integrateSubstitution fs v (UnaryApply func (Symbol v)) =
      Just $ (c :/: d) :*: substitute e (\s -> if s == v then Just y else Nothing)
  | otherwise = Nothing
  where
    (c, x') = factor v x
    (d, y') = factor v $ differentiate v y
integrate fs v (e@(UnaryApply _ _) :*: x) = integrate fs v $ x :*: e
integrate fs v e@(UnaryApply _ _) = integrate fs v $ Number 1 :*: e
integrate fs v (x :*: BinaryApply func y z)
  | c /= Number 0,
    x' == y',
    isConstant v z,
    Just e <- integrateSubstitution fs v (BinaryApply func (Symbol v) z) =
      Just $ (b :/: c) :*: substitute e (\s -> if s == v then Just y else Nothing)
  | d /= Number 0,
    x' == z',
    isConstant v y,
    Just e <- integrateSubstitution fs v (BinaryApply func y (Symbol v)) =
      Just $ (b :/: d) :*: substitute e (\s -> if s == v then Just z else Nothing)
  | otherwise = Nothing
  where
    (b, x') = factor v x
    (c, y') = factor v $ differentiate v y
    (d, z') = factor v $ differentiate v z
integrate fs v (e@(BinaryApply _ _ _) :*: x) = integrate fs v $ x :*: e
integrate fs v e@(BinaryApply func _ _)
  | func /= Multiply = integrate fs v $ Number 1 :*: e
  | otherwise = Nothing
integrate _ _ _ = Nothing

integrateSubstitution :: [Text -> Expression -> Maybe Expression] -> Text -> Expression -> Maybe Expression
integrateSubstitution fs v e = asum $ map (\f -> f v e) fs
