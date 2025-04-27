module Symkell.Integration.Rational
  ( integrate,
    hermiteReduce,
    rationalIntegralLogTerms,
    complexLogTermToAtan,
    complexLogTermToRealTerm,
    RationalFunction,
  )
where

import Data.Foldable (asum)
import Data.List (find, intersect)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Symkell.Polynomial hiding (integrate)
import Symkell.Polynomial qualified as Polynomial
import Symkell.Polynomial.Indexed
import Symkell.Polynomial.Rational as Rational
import Symkell.Polynomial.Solve
import Symkell.Polynomial.Symbolic
import Symkell.Symbolic
import Symkell.Symbolic.Simplify

integrate :: Text -> Expression -> Maybe Expression
integrate v e
  | (x :/: y) <- e',
    (Just n) <- fromExpression (forVariable v) x,
    (Just d) <- fromExpression (forVariable v) y,
    d /= 0 =
      integrate' n d
  | otherwise = Nothing
  where
    e' = simplifyForVariable v e
    integrate' n d = (+) reduced . (+) poly <$> logs
      where
        (g, h) = hermiteReduce $ fromPolynomials n d
        reduced = sum $ map fromRationalFunction g
        Rational.Function numer denom = h
        (q, r) = numer `divide` denom
        poly = toExpression v toRationalCoefficient $ Polynomial.integrate q
        h' = fromPolynomials r denom
        logTerms = rationalIntegralLogTerms h'
        logs = asum [realLogs, complexLogs] :: Maybe Expression
        realLogs
          | (Just terms) <- logTerms = sum <$> mapM (complexLogTermToRealExpression v) terms
          | otherwise = Nothing
        complexLogs
          | (Just terms) <- logTerms = sum <$> mapM (complexLogTermToComplexExpression v) terms
          | otherwise = Nothing
        fromRationalFunction (Rational.Function u w) = u' / w'
          where
            u' = toExpression v toRationalCoefficient u
            w' = toExpression v toRationalCoefficient w

type RationalFunction = Rational.Function IndexedPolynomial

hermiteReduce :: RationalFunction -> ([RationalFunction], RationalFunction)
hermiteReduce h@(Rational.Function _ 0) = ([], h)
hermiteReduce h@(Rational.Function x y)
  | (Just z) <- reduce x [] common = z
  | otherwise = ([], h)
  where
    common = monic $ greatestCommonDivisor y $ differentiate y
    (divisor, _) = y `divide` common
    reduce a g d
      | degree d > 0 = do
          let d' = monic $ greatestCommonDivisor d $ differentiate d
          let (d'', _) = d `divide` d'
          let (d''', _) = (divisor * differentiate d) `divide` d
          (b, c) <- diophantineEuclidean (-d''') d'' a
          let (b', _) = (differentiate b * divisor) `divide` d''
          let a' = c - b'
          let g' = fromPolynomials b d : g
          reduce a' g' d'
      | otherwise = Just (g, fromPolynomials a divisor)

rationalIntegralLogTerms ::
  RationalFunction ->
  Maybe [(IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial)]
rationalIntegralLogTerms (Rational.Function a d) = do
  let sa = mapCoefficients fromRational a
  let sd = mapCoefficients fromRational d
  let t = fromPolynomial $ power 1
  let (resultant, prs) = subresultant sd $ sa - scale t (differentiate sd)
  sd' <- mapCoefficientsM toPolynomial sd
  resultant' <- toPolynomial resultant
  prs' <- mapM (mapCoefficientsM toPolynomial) prs :: Maybe [IndexedPolynomialWith IndexedPolynomial]
  let qs = squarefree resultant' :: [IndexedPolynomial]
  let terms = zipWith (toTerm sd' prs') [1 ..] qs
  return $ filter ((/=) 1 . snd) terms
  where
    toTerm ::
      IndexedPolynomialWith IndexedPolynomial ->
      [IndexedPolynomialWith IndexedPolynomial] ->
      Int ->
      IndexedPolynomial ->
      (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial)
    toTerm sd prs i q
      | degree q == 0 = (q, 1)
      | i == degree d = (q, sd)
      | (Just r) <- find ((==) i . degree) prs = derive q r
      | otherwise = (q, 1)
    derive ::
      IndexedPolynomial ->
      IndexedPolynomialWith IndexedPolynomial ->
      (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial)
    derive q s = (q, s')
      where
        as = squarefree $ leadingCoefficient s
        s' = foldl scalePoly s (zip ([1 ..] :: [Int]) as)
          where
            scalePoly x (j, u) =
              getSum $ foldTerms (reduceTerm (monic $ greatestCommonDivisor u q ^ j)) x
            reduceTerm v e c = Sum $ scale (exactDivide c v) $ power e
            exactDivide u v = r
              where
                (r, _) = u `divide` v

complexLogTermToAtan ::
  Text ->
  IndexedPolynomial ->
  IndexedPolynomial ->
  Expression
complexLogTermToAtan v a b
  | r == 0 = 2 * atan (a' / b')
  | degree a < degree b = complexLogTermToAtan v (-b) a
  | otherwise = 2 * atan (s' / g') + complexLogTermToAtan v d c
  where
    (_, r) = a `divide` b
    (d, c, g) = extendedEuclidean b (-a)
    a' = toExpression v toRationalCoefficient a
    b' = toExpression v toRationalCoefficient b
    g' = toExpression v toRationalCoefficient g
    s' = toExpression v toRationalCoefficient $ a * d + b * c

complexLogTermToRealTerm ::
  (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial) ->
  ( (IndexedPolynomialWith IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial),
    (IndexedPolynomialWith (IndexedPolynomialWith IndexedPolynomial), IndexedPolynomialWith (IndexedPolynomialWith IndexedPolynomial))
  )
complexLogTermToRealTerm (q, s) = ((qp, qq), (sp, sq))
  where
    q' = getSum $ foldTerms reduceImaginary $ getSum $ foldTerms fromTerm q
      where
        fromTerm :: Int -> Rational -> Sum (IndexedPolynomialWith (IndexedPolynomialWith IndexedPolynomial))
        fromTerm e c = Sum $ c' * (u + i * v) ^ e
          where
            c' = scale (scale (scale c 1) 1) 1
        i = power 1
        u = scale (power 1) 1
        v = scale (scale (power 1) 1) 1
    (qp, qq) = (coefficient q' 0, coefficient q' 1)
    s' = getSum $ foldTerms reduceImaginary $ getSum $ foldTerms fromTerm s
      where
        fromTerm :: Int -> IndexedPolynomial -> Sum (IndexedPolynomialWith (IndexedPolynomialWith (IndexedPolynomialWith IndexedPolynomial)))
        fromTerm e c = Sum $ c' * x ^ e
          where
            c' = getSum $ foldTerms fromCoefficient c
            fromCoefficient e' c'' = Sum $ c''' * (u + i * v) ^ e'
              where
                c''' = scale (scale (scale (scale c'' 1) 1) 1) 1
        i = power 1
        x = scale (power 1) 1
        u = scale (scale (power 1) 1) 1
        v = scale (scale (scale (power 1) 1) 1) 1
    (sp, sq) = (coefficient s' 0, coefficient s' 1)
    reduceImaginary :: (Eq a, Num a) => Int -> a -> Sum (IndexedPolynomialWith a)
    reduceImaginary e c = Sum $ case e `mod` 4 of
      0 -> c'
      1 -> c' * i
      2 -> c' * (-1)
      3 -> c' * (-i)
      _ -> 0
      where
        i = power 1
        c' = scale c 1

complexLogTermToRealExpression ::
  Text ->
  (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial) ->
  Maybe Expression
complexLogTermToRealExpression v (r, s)
  | (Just xys) <- solveBivariatePolynomials p q,
    (Just h) <- f xys,
    (Just zs) <- toRationalList (solve r) =
      Just $ sum h + g zs
  | otherwise = Nothing
  where
    ((p, q), (a, b)) = complexLogTermToRealTerm (r, s)
    f :: [(Rational, Rational)] -> Maybe [Expression]
    f xys = sequence $ do
      (x, y) <- filter ((> 0) . snd) xys
      let flatten'' = mapCoefficients (toExpr (fromRational y) fromRational)
      let flatten' = mapCoefficients (toExpr (fromRational x) id . flatten'')
      let flatten = toExpr (Symbol v) id . flatten'
      let a' = flatten a
      let b' = flatten b
      return $ do
        a'' <- convertCoefficients $ flatten' a
        b'' <- convertCoefficients $ flatten' b
        return $ fromRational x * log (a' * a' + b' * b') + fromRational y * complexLogTermToAtan v a'' b''
    g zs = sum $ do
      z <- zs
      let s' = mapCoefficients (toExpr (fromRational z) fromRational) s
      return $ fromRational z * Log' (toExpression v toSymbolicCoefficient s')
    toRationalList :: Maybe [Expression] -> Maybe [Rational]
    toRationalList Nothing = Nothing
    toRationalList (Just []) = Just []
    toRationalList (Just (x : xs))
      | (Just x'') <- convert (simplify x'), (Just xs'') <- xs' = Just $ x'' : xs''
      | otherwise = Nothing
      where
        x' = simplify x
        xs' = toRationalList $ Just xs
    convert (Number n) = Just $ fromIntegral n
    convert (Number n :/: Number m) = Just $ fromIntegral n / fromIntegral m
    convert _ = Nothing
    convertCoefficients :: IndexedPolynomialWith Expression -> Maybe IndexedPolynomial
    convertCoefficients x = sum . map (\(e, c) -> scale c (power e)) <$> sequence (foldTerms (\e c -> [(e,) <$> convert (simplify c)]) x)
    toExpr x h u = getSum $ foldTerms (\e'' c -> Sum $ h c * (x ** Number (fromIntegral e''))) u

complexLogTermToComplexExpression ::
  Text ->
  (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial) ->
  Maybe Expression
complexLogTermToComplexExpression v (q, s) = do
  as <- complexSolve q
  let terms = do
        a <- as
        let s' = mapCoefficients (collapse a) s
        let s'' = toExpression v toSymbolicCoefficient s'
        return $ a * log s''
  return $ sum terms
  where
    collapse a c' = getSum $ foldTerms (\e c -> Sum $ fromRational c * a ** fromIntegral e) c'

solveBivariatePolynomials ::
  IndexedPolynomialWith IndexedPolynomial ->
  IndexedPolynomialWith IndexedPolynomial ->
  Maybe [(Rational, Rational)]
solveBivariatePolynomials p q = do
  let p' = toRationalFunctionCoefficients p
  let q' = toRationalFunctionCoefficients q
  resultant <- toPolynomial $ fst $ subresultant p' q'
  vs' <- solve resultant
  vs <- mapM (convert . simplify) vs'
  concat <$> mapM solveForU vs
  where
    toRationalFunctionCoefficients = mapCoefficients fromPolynomial
    solveForU :: Rational -> Maybe [(Rational, Rational)]
    solveForU v
      | 0 <- p' = do
          u <- map (convert . simplify) <$> solve q'
          map (,v) <$> sequence u
      | 0 <- q' = do
          u <- map (convert . simplify) <$> solve p'
          map (,v) <$> sequence u
      | otherwise = do
          up <- map (convert . simplify) <$> solve p'
          uq <- map (convert . simplify) <$> solve q'
          up' <- sequence up
          uq' <- sequence uq
          return $ map (,v) $ up' `intersect` uq'
      where
        p' = mapCoefficients (getSum . foldTerms (\e c -> Sum $ c * v ^ e)) p
        q' = mapCoefficients (getSum . foldTerms (\e c -> Sum $ c * v ^ e)) q
    convert :: Expression -> Maybe Rational
    convert (Number n) = Just $ fromIntegral n
    convert (Number n :/: Number m) = Just $ fromIntegral n / fromIntegral m
    convert _ = Nothing
