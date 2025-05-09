module Symkell.Polynomial.Differential
  ( canonical,
    splitFactor,
    splitSquarefreeFactor,
    extend,
    consistent,
  )
where

import Data.Monoid (Sum (..))
import Symkell.Polynomial
import Symkell.Polynomial.Rational

splitSquarefreeFactor ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  p e c ->
  [(p e c, p e c)]
splitSquarefreeFactor derivation p = map split ps
  where
    ps = squarefree p
    split q = (qn, qs)
      where
        qs = greatestCommonDivisor q $ derivation q
        (qn, _) = q `divide` qs

splitFactor ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  p e c ->
  (p e c, p e c)
splitFactor derivation p = compose (1, 1) factors (1 :: Int)
  where
    factors = splitSquarefreeFactor derivation p
    compose split [] _ = split
    compose (pn, ps) ((qn, qs) : fs) n = compose (pn * (qn ^ n), ps * (qs ^ n)) fs (n + 1)

canonical ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  Function (p e c) ->
  (p e c, Function (p e c), Function (p e c))
canonical _ x@(Function _ 0) = (0, 0, x)
canonical derivation (Function a d)
  | Just (b, c) <- xs = (q, fromPolynomials c dn, fromPolynomials b ds)
  | otherwise = (q, 0, fromPolynomials r d)
  where
    a' = scale (1 / leadingCoefficient d) a
    d' = scale (1 / leadingCoefficient d) d
    (q, r) = a' `divide` d'
    (dn, ds) = splitFactor derivation d'
    xs = diophantineEuclidean dn ds r

consistent ::
  (Eq a, Num a) =>
  (a -> a) ->
  a ->
  a ->
  Bool
consistent derivation a b = additive && productive
  where
    additive = derivation (a + b) == derivation a + derivation b
    productive = derivation (a * b) == a * derivation b + b * derivation a

extend :: (Polynomial p e c, Num (p e c), Eq c, Num c) => (c -> c) -> p e c -> p e c -> p e c
extend derivation w p = getSum $ foldTerms (\e c -> Sum $ derive e c) p
  where
    derive e c = scale (derivation c) (power e) + scale (c * fromIntegral e) (power $ e - 1) * w
