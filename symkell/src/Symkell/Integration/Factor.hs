module Symkell.Integration.Factor (factor, isConstant) where

import Data.Text (Text)
import Symkell.Symbolic
import Symkell.Symbolic.Simplify

factor ::
  Text ->
  Expression ->
  (Expression, Expression)
factor _ e@(Number _) = (e, Number 1)
factor v e@(Symbol s) | v == s = (Number 1, e) | otherwise = (e, Number 1)
factor v e@(UnaryApply _ x) | isConstant v x = (e, Number 1) | otherwise = (Number 1, e)
factor v e@(x :*: (y :*: z))
  | isConstant v x, isConstant v y, isConstant v z = (e, Number 1)
  | isConstant v x, isConstant v y = (simplifyForVariable v $ x :*: (y :*: c), z')
  | isConstant v x = (simplifyForVariable v $ x :*: d, y')
  | otherwise = (Number 1, e)
  where
    (c, z') = factor v z
    (d, y') = factor v $ y :*: z
factor v e@(x :*: y)
  | isConstant v x, isConstant v y = (e, Number 1)
  | isConstant v x = (x, y)
  | otherwise = (Number 1, e)
factor v (x :/: y) = (simplify $ constX :/: constY, simplify $ varX :/: varY)
  where
    (constX, varX) = factor v x
    (constY, varY) = factor v y
factor v e | isConstant v e = (e, Number 1) | otherwise = (Number 1, e)

isConstant ::
  Text ->
  Expression ->
  Bool
isConstant _ (Number _) = True
isConstant v (Symbol s) = s /= v
isConstant v (UnaryApply _ x) = isConstant v x
isConstant v (BinaryApply _ x y) = isConstant v x && isConstant v y
