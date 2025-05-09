module Symkell.Symbolic.Simplify (simplify, tidy, simplifyForVariable) where

import Data.Text (Text)
import Symkell.Symbolic
import Symkell.Symbolic.Simplify.AlgebraicRingOrder qualified as AlgebraicRingOrder
import Symkell.Symbolic.Simplify.Fraction qualified as Fraction
import Symkell.Symbolic.Simplify.NumericFolding qualified as NumericFolding
import Symkell.Symbolic.Simplify.SymbolicFolding qualified as SymbolicFolding
import Symkell.Symbolic.Simplify.Tidy

simplify :: Expression -> Expression
simplify = simplifyForVariable ""

simplifyForVariable ::
  Text ->
  Expression ->
  Expression
simplifyForVariable v e
  | e == e' = e
  | otherwise = simplifyForVariable v e'
  where
    e' = f e
    f = Fraction.simplify . NumericFolding.simplify . SymbolicFolding.simplify . AlgebraicRingOrder.order v
