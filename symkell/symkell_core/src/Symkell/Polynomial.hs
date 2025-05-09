module Symkell.Polynomial
  ( Polynomial (..),
    monic,
    mapCoefficients,
    mapCoefficientsM,
    divide,
    pseudoDivide,
    extendedEuclidean,
    diophantineEuclidean,
    greatestCommonDivisor,
    subresultant,
    differentiate,
    integrate,
    squarefree,
  )
where

import Data.Monoid (Sum (..))

class (Integral e, Num c) => Polynomial p e c where
  degree :: p e c -> e
  coefficient :: p e c -> e -> c
  leadingCoefficient :: p e c -> c
  deleteLeadingTerm :: p e c -> p e c
  foldTerms :: (Monoid m) => (e -> c -> m) -> p e c -> m
  scale :: c -> p e c -> p e c
  power :: e -> p e c

monic :: (Polynomial p e c, Eq c, Fractional c) => p e c -> p e c
monic p
  | leadingCoefficient p == 0 = p
  | otherwise = scale (1 / leadingCoefficient p) p

mapCoefficients ::
  (Polynomial p e c, Polynomial p e c', Num (p e c), Num (p e c')) =>
  (c -> c') ->
  p e c ->
  p e c'
mapCoefficients f p = getSum $ foldTerms convertTerm p
  where
    convertTerm e c = Sum $ scale (f c) (power e)

mapCoefficientsM ::
  (Polynomial p e c, Polynomial p e c', Num (p e c), Num (p e c'), Monad m) =>
  (c -> m c') ->
  p e c ->
  m (p e c')
mapCoefficientsM f p = sum <$> mapM f' terms
  where
    terms = foldTerms (\e c -> [(e, c)]) p
    f' (e, c) = do
      c' <- f c
      return $ scale c' $ power e

divide ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  p e c ->
  p e c ->
  (p e c, p e c)
divide p q = go 0 p
  where
    go quotient remainder
      | remainder /= 0, delta >= 0 = go (quotient + t) (remainder' - qt')
      | otherwise = (quotient, remainder)
      where
        delta = degree remainder - degree q
        t = scale (leadingCoefficient remainder / leadingCoefficient q) $ power delta
        remainder' = deleteLeadingTerm remainder
        qt' = deleteLeadingTerm $ q * t

pseudoDivide ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Num c) =>
  p e c ->
  p e c ->
  (p e c, p e c)
pseudoDivide p q
  | degree p < degree q = (0, p)
  | otherwise = go (1 + degree p - degree q) 0 p
  where
    b = leadingCoefficient q
    go n quotient remainder
      | remainder /= 0, delta >= 0 = go (n - 1) quotient' remainder'
      | otherwise = (scale (b ^ n) quotient, scale (b ^ n) remainder)
      where
        delta = degree remainder - degree q
        t = scale (leadingCoefficient remainder) (power delta)
        quotient' = scale b quotient + t
        remainder' = deleteLeadingTerm (scale b remainder) - deleteLeadingTerm (t * q)

extendedEuclidean ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  p e c ->
  p e c ->
  (p e c, p e c, p e c)
extendedEuclidean u v = descend u v 1 0 0 1
  where
    descend g 0 s t _ _ = (s, t, g)
    descend a b a1 a2 b1 b2 = descend b r b1 b2 r1 r2
      where
        (q, r) = divide a b
        r1 = a1 - q * b1
        r2 = a2 - q * b2

diophantineEuclidean ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  p e c ->
  p e c ->
  p e c ->
  Maybe (p e c, p e c)
diophantineEuclidean a b c
  | r /= 0 = Nothing
  | s' /= 0, degree s' >= degree b = Just (r', t' + q' * a)
  | otherwise = Just (s', t')
  where
    (s, t, g) = extendedEuclidean a b
    (q, r) = divide c g
    s' = q * s
    t' = q * t
    (q', r') = divide s' b

greatestCommonDivisor ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  p e c ->
  p e c ->
  p e c
greatestCommonDivisor p q = g
  where
    (_, _, g) = extendedEuclidean p q

subresultant ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Num e, Fractional c) =>
  p e c ->
  p e c ->
  (c, [p e c])
subresultant p q
  | degree p >= degree q = (resultantFromSequence rs betas, rs)
  | otherwise = ((-1) ^ (degree q * degree p) * resultant, prs)
  where
    (rs, betas) = subresultantRemainderSequence (p, q) gamma beta
    gamma = -1
    beta = (-1) ^ (1 + delta)
    delta = degree p - degree q
    (resultant, prs) = subresultant q p

subresultantRemainderSequence ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Num e, Fractional c) =>
  (p e c, p e c) ->
  c ->
  c ->
  ([p e c], [c])
subresultantRemainderSequence (rprev, rcurr) gamma beta
  | rcurr /= 0 = (rprev : rs, beta : betas)
  | otherwise = ([rprev, rcurr], [beta])
  where
    (rs, betas) = subresultantRemainderSequence (rcurr, rnext) gamma' beta'
    (_, r) = pseudoDivide rprev rcurr
    rnext = scale (1 / beta) r
    lc = leadingCoefficient rcurr
    delta = degree rprev - degree rcurr
    delta' = degree rcurr - degree rnext
    gamma' = ((-lc) ^ delta) * (gamma ^^ (1 - delta))
    beta' = (-lc) * (gamma' ^ delta')

resultantFromSequence ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Num e, Fractional c) =>
  [p e c] ->
  [c] ->
  c
resultantFromSequence rs betas = go rs betas 1 1
  where
    go (r : r' : r'' : rs') (beta : betas') c s
      | [] <- rs', degree r' > 0 = 0
      | [] <- rs', degree r == 1 = leadingCoefficient r'
      | [] <- rs' = s * c * leadingCoefficient r' ^ degree r
      | otherwise = go (r' : r'' : rs') betas' c' s'
      where
        s' | odd (degree r), odd (degree r') = -s | otherwise = s
        c' = c * ((beta / (lc ^ (1 + delta))) ^ degree r') * (lc ^ (degree r - degree r''))
        lc = leadingCoefficient r'
        delta = degree r - degree r'
    go _ _ _ _ = 0

differentiate :: (Polynomial p e c, Num (p e c), Num c) => p e c -> p e c
differentiate p = getSum $ foldTerms diffTerm p
  where
    diffTerm 0 _ = Sum 0
    diffTerm e c = Sum $ scale (fromIntegral e * c) $ power (e - 1)

integrate :: (Polynomial p e c, Num (p e c), Fractional c) => p e c -> p e c
integrate p = getSum $ foldTerms integrateTerm p
  where
    integrateTerm e c = Sum $ scale (c / (1 + fromIntegral e)) $ power (e + 1)

squarefree :: (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) => p e c -> [p e c]
squarefree 0 = [0]
squarefree p
  | (x : xs) <- factor u v = scale c x : xs
  | otherwise = [scale c 1]
  where
    c = leadingCoefficient p
    q = scale (1 / c) p
    q' = differentiate q
    g = monic $ greatestCommonDivisor q q'
    (u, _) = q `divide` g
    (v, _) = q' `divide` g
    factor s y
      | z == 0 = [s]
      | otherwise = f : factor s' y'
      where
        z = y - differentiate s
        f = monic $ greatestCommonDivisor s z
        (s', _) = s `divide` f
        (y', _) = z `divide` f
