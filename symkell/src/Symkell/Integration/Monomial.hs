module Symkell.Integration.Monomial
  ( hermiteReduce,
    polynomialReduce,
    residueReduce,
  )
where

import Data.List (find)
import Data.Monoid (Sum (..))
import Symkell.Polynomial
import Symkell.Polynomial.Differential
import Symkell.Polynomial.Rational

hermiteReduce ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  Function (p e c) ->
  ([Function (p e c)], Function (p e c), Function (p e c))
hermiteReduce _ r@(Function _ 0) = ([], 0, r)
hermiteReduce derivation f = (g, h, fromPolynomial (q + p) + s)
  where
    (p, n, s) = canonical derivation f
    (g, h, q) = hermiteReduce' derivation n

hermiteReduce' ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  Function (p e c) ->
  ([Function (p e c)], Function (p e c), p e c)
hermiteReduce' derivation f@(Function x y)
  | (Just (g, (a, d))) <- reduce x [] common =
      let (q, r) = a `divide` d
       in (g, fromPolynomials r d, q)
  | otherwise = ([], f, 0)
  where
    common = monic $ greatestCommonDivisor y $ derivation y
    (divisor, _) = y `divide` common
    reduce a g d
      | degree d > 0 = do
          let d' = monic $ greatestCommonDivisor d $ derivation d
          let (d'', _) = d `divide` d'
          let (d''', _) = (divisor * derivation d) `divide` d
          (b, c) <- diophantineEuclidean (-d''') d'' a
          let (b', _) = (derivation b * divisor) `divide` d''
          let a' = c - b'
          let g' = fromPolynomials b d : g
          reduce a' g' d'
      | otherwise = Just (g, (a, divisor))

polynomialReduce ::
  (Polynomial p e c, Num (p e c), Fractional c) =>
  (p e c -> p e c) ->
  p e c ->
  (p e c, p e c)
polynomialReduce derivation p
  | delta == 0 = (0, p)
  | degree p < delta = (0, p)
  | otherwise = (q0 + q, r)
  where
    delta = degree $ derivation $ power 1
    lambda = leadingCoefficient $ derivation $ power 1
    m = degree p - delta + 1
    q0 = scale (leadingCoefficient p / (fromIntegral m * lambda)) (power m)
    (q, r) = polynomialReduce derivation $ deleteLeadingTerm p - deleteLeadingTerm (derivation q0)

residueReduce ::
  ( Polynomial p e c,
    Eq (p e c),
    Num (p e c),
    Polynomial p e (p e c),
    Eq (p e (p e c)),
    Num (p e (p e c)),
    Polynomial p e (Function (p e c)),
    Eq (p e (Function (p e c))),
    Num (p e (Function (p e c))),
    Eq c,
    Fractional c
  ) =>
  (p e c -> p e c) ->
  Function (p e c) ->
  Maybe ([(p e c, p e (p e c))], Bool)
residueReduce derivation (Function e d) = do
  let (_, a) = e `divide` d
  let d' = mapCoefficients (\c -> fromPolynomial $ scale c 1) d
  let a' = mapCoefficients (\c -> fromPolynomial $ scale c 1) a
  let zd' = mapCoefficients (\c -> fromPolynomial $ scale c $ power 1) (derivation d)
  let (r', rs')
        | degree (derivation d) <= degree d = subresultant d' (a' - zd')
        | otherwise = subresultant (a' - zd') d'
  r <- toPolynomial r'
  rs <- mapM (mapCoefficientsM toPolynomial) rs'
  let kderiv = getSum . foldTerms (\ex c -> Sum $ derivation (scale c 1) * power ex)
  let factors = splitSquarefreeFactor kderiv r
  let elementary = all ((==) 0 . degree . fst) factors
  let specials = map snd factors
  terms' <- mapM (toTerm rs specials) $ zip [1 ..] specials
  let terms = filter ((/= 1) . snd) terms'
  return (terms, elementary)
  where
    toTerm prs specials (i, s)
      | degree s == 0 = Just (monic s, 1)
      | degree d == i = Just (monic s, mapCoefficients (`scale` 1) d)
      | Just pr <- find ((==) i . degree) prs = derive s pr specials
      | otherwise = Nothing
    derive s pr specials = do
      let pr' = mapCoefficients fromPolynomial $ switchVars pr
      let (logArg', _) = pr' `divide` divisor
      logArg <- mapCoefficientsM toPolynomial logArg'
      return (monic s, switchVars logArg)
      where
        factors = squarefree $ leadingCoefficient pr
        divisor = mapCoefficients (fromPolynomial . flip scale 1) divisor'
          where
            divisor' = product $ map toDivisor $ zip3 [1 ..] factors specials
            toDivisor (j, factor, special) = greatestCommonDivisor factor special ^ (j :: Int)

switchVars ::
  (Polynomial p e (p e c), Polynomial p e c, Num (p e (p e c))) =>
  p e (p e c) ->
  p e (p e c)
switchVars p = getSum $ foldTerms (\e c -> Sum $ mapCoefficients (\c' -> scale c' $ power e) c) p
