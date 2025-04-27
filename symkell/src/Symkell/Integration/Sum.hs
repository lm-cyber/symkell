module Symkell.Integration.Sum (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symkell.Symbolic

integrate ::
  [Text -> Expression -> Maybe Expression] ->
  Text ->
  Expression ->
  Maybe Expression
integrate fs v (Negate' x) =
  UnaryApply Negate <$> integrate fs v x
integrate fs v (x :-: y) =
  integrate fs v (x :+: Negate' y)
integrate fs v (x@(_ :+: _) :+: y@(_ :+: _)) =
  BinaryApply Add <$> integrate fs v x <*> integrate fs v y
integrate fs v (x@(_ :+: _) :+: y) =
  BinaryApply Add <$> integrate fs v x <*> asum [f v y | f <- fs]
integrate fs v (x :+: y@(_ :+: _)) =
  BinaryApply Add <$> asum [f v x | f <- fs] <*> integrate fs v y
integrate fs v (x :+: y) =
  BinaryApply Add <$> asum [f v x | f <- fs] <*> asum [f v y | f <- fs]
integrate _ _ _ = Nothing
