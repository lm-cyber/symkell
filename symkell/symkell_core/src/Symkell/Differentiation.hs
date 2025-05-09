module Symkell.Differentiation (differentiate) where

import Data.Text (Text)
import Numeric.AD.Rank1.Forward
import Symkell.Symbolic
import Symkell.Symbolic.Simplify

differentiate ::
  Text ->
  Expression ->
  Expression
differentiate v e = tidy $ simplifyForVariable v $ diff f $ Symbol v
  where
    f = toFunction e assign
    assign x
      | v == x = id
      | otherwise = const $ auto $ Symbol x
