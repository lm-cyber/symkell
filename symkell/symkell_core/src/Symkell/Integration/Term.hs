module Symkell.Integration.Term (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symkell.Integration.Factor
import Symkell.Symbolic
import Symkell.Symbolic.Simplify

integrate ::
  [Text -> Expression -> Maybe Expression] ->
  Text ->
  Expression ->
  Maybe Expression
integrate fs v e = asum $ map (\f -> (:*:) c <$> f v u) fs
  where
    e' = simplifyForVariable v e
    (c, u) = factor v e'
