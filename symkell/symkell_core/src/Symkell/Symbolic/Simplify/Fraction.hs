module Symkell.Symbolic.Simplify.Fraction (simplify) where

import Symkell.Symbolic

simplify :: Expression -> Expression
simplify e@(_ :/: Number 0) = e
simplify (Number n :/: Number m)
  | m > 0 = Number (n `div` g) :/: Number (m `div` g)
  | otherwise = Number ((-n) `div` g) :/: Number ((-m) `div` g)
  where
    g = gcd n m
simplify (x :/: y) = divideFactor g x' :/: divideFactor g y'
  where
    g
      | (Number n) <- y, n < 0 = negate $ gcd (commonFactor x') n
      | otherwise = gcd (commonFactor x') (commonFactor y')
    x' = simplify x
    y' = simplify y
simplify ((1 :/: x) :*: y) = (1 :/: divideFactor g x') :*: divideFactor g y'
  where
    g = gcd (commonFactor x') (commonFactor y')
    x' = simplify x
    y' = simplify y
simplify (UnaryApply func x) = UnaryApply func $ simplify x
simplify (BinaryApply func x y) = BinaryApply func (simplify x) (simplify y)
simplify e = e

commonFactor :: Expression -> Integer
commonFactor (Number n) = n
commonFactor (x :+: y) = gcd (commonFactor x) (commonFactor y)
commonFactor (x :-: y) = gcd (commonFactor x) (commonFactor y)
commonFactor (Number n :*: _) = n
commonFactor _ = 1

divideFactor :: Integer -> Expression -> Expression
divideFactor 0 e = e
divideFactor 1 e = e
divideFactor g (Number n) = Number $ n `div` g
divideFactor g (x :+: y) = divideFactor g x :+: divideFactor g y
divideFactor g (Number n :*: x) = Number (n `div` g) :*: x
divideFactor g e = e :/: Number g
