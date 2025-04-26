module Symkell.Polynomial.Solve (solve, complexSolve) where

import Data.List (nub)
import Symkell.Polynomial
import Symkell.Polynomial.Indexed
import Symkell.Symbolic

solve :: IndexedPolynomial -> Maybe [Expression]
solve p
  | degree p == 1 = solveLinear (c 1) (c 0)
  | degree p == 2 = solveQuadratic (c 2) (c 1) (c 0)
  | degree p == 3 = solveCubic (c 3) (c 2) (c 1) (c 0)
  | degree p == 4 = solveQuartic (c 4) (c 3) (c 2) (c 1) (c 0)
  | otherwise = Nothing
  where
    c = coefficient p

solveLinear :: Rational -> Rational -> Maybe [Expression]
solveLinear a b = Just [fromRational ((-b) / a)]

solveQuadratic :: Rational -> Rational -> Rational -> Maybe [Expression]
solveQuadratic a b c
  | sq == 0 = Just [fromRational $ (-b) / (2 * a)]
  | sq > 0 =
      Just
        [ ((-b') + sq' ** (1 / 2)) / (2 * a'),
          ((-b') - sq' ** (1 / 2)) / (2 * a')
        ]
  | otherwise = Just []
  where
    sq = b * b - 4 * a * c
    sq' = fromRational sq
    a' = fromRational a
    b' = fromRational b

solveCubic :: Rational -> Rational -> Rational -> Rational -> Maybe [Expression]
solveCubic a b c d = map restore <$> depressedRoots
  where
    restore x = x - fromRational b / (3 * fromRational a)
    depressedRoots = solveDepressedCubic p q
    p = (3 * a * c - b ^ two) / (3 * a ^ two)
    q = (2 * b ^ three - 9 * a * b * c + 27 * a ^ two * d) / (27 * a ^ three)
    two = 2 :: Int
    three = 3 :: Int

solveDepressedCubic :: Rational -> Rational -> Maybe [Expression]
solveDepressedCubic 0 q
  | q < 0 = Just [fromRational (-q) ** (1 / 3)]
  | otherwise = Just [negate $ fromRational q ** (1 / 3)]
solveDepressedCubic p q
  | s < 0 =
      let c = 2 * sqrt (-(p' / 3))
          theta = acos (3 / 2 * q' / p' * sqrt (-(3 / p'))) / 3
       in Just [c * cos theta, c * cos (theta - 2 * pi / 3), c * cos (theta - 4 * pi / 3)]
  | p < 0,
    s > 0 =
      Just [(-2) * signum q' * sqrt (-(p' / 3)) * cosh (acosh ((-3) / 2 * abs q' / p' * sqrt (-(3 / p'))) / 3)]
  | s == 0 = Just [fromRational (3 * q / p), fromRational ((-3) / 2 * q / p)]
  | p > 0 =
      Just [(-2) * sqrt (p' / 3) * sinh (asinh (3 / 2 * q' / p' * sqrt (3 / p')) / 3)]
  | otherwise = Nothing
  where
    s = 4 * p ^ (3 :: Int) + 27 * q ^ (2 :: Int)
    p' = fromRational p
    q' = fromRational q

solveQuartic :: Rational -> Rational -> Rational -> Rational -> Rational -> Maybe [Expression]
solveQuartic a b 0 0 0
  | b /= 0 = Just [0, fromRational $ -(b / a)]
  | otherwise = Just [0]
solveQuartic a b c 0 0
  | (Just xs) <- solveQuadratic a b c = Just $ nub $ 0 : xs
  | otherwise = Nothing
solveQuartic a b c d 0
  | (Just xs) <- solveCubic a b c d = Just $ nub $ 0 : xs
  | otherwise = Nothing
solveQuartic a 0 0 0 b
  | a > 0, b > 0 = Just []
  | a < 0, b < 0 = Just []
  | b == 0 = Just [0]
  | otherwise = Just [x, -x]
  where
    x = fromRational ((-b) / a) ** (1 / 4)
solveQuartic a 0 b 0 c
  | sq < 0 = Just []
  | sq == 0, st < 0 = Just []
  | sq == 0 = Just [sqrt st', -sqrt st']
  | a > 0, sq > 0, b > 0, sq > b * b = Just [sqrt x1, -sqrt x1]
  | a < 0, sq > 0, b < 0, sq > b * b = Just [sqrt x2, -sqrt x2]
  | a > 0, sq > 0, b < 0, sq < b * b = Just [sqrt x1, -sqrt x1, sqrt x2, -sqrt x2]
  | a < 0, sq > 0, b > 0, sq < b * b = Just [sqrt x1, -sqrt x1, sqrt x2, -sqrt x2]
  | otherwise = Nothing
  where
    sq = b * b - 4 * a * c
    st = (-b) / (2 * a)
    sq' = fromRational sq
    st' = fromRational st
    a' = fromRational a
    b' = fromRational b
    x1 = ((-b') + sqrt sq') / (2 * a')
    x2 = ((-b') - sqrt sq') / (2 * a')
solveQuartic _ _ _ _ _ = Nothing

complexSolve :: IndexedPolynomial -> Maybe [Expression]
complexSolve p
  | degree p == 1 = complexSolveLinear (c 1) (c 0)
  | degree p == 2 = complexSolveQuadratic (c 2) (c 1) (c 0)
  | degree p == 3 = complexSolveCubic (c 3) (c 2) (c 1) (c 0)
  | degree p == 4 = complexSolveQuartic (c 4) (c 3) (c 2) (c 1) (c 0)
  | otherwise = Nothing
  where
    c = coefficient p

complexSolveLinear :: Rational -> Rational -> Maybe [Expression]
complexSolveLinear a b = Just [fromRational $ (-b) / a]

complexSolveQuadratic :: Rational -> Rational -> Rational -> Maybe [Expression]
complexSolveQuadratic a b c
  | sq == 0 = Just [p]
  | otherwise = Just [p + q, p - q]
  where
    sq = b * b - 4 * a * c
    p = fromRational $ (-b) / (2 * a)
    q = sqrt (fromRational sq) / fromRational (2 * a)

complexSolveCubic :: Rational -> Rational -> Rational -> Rational -> Maybe [Expression]
complexSolveCubic _ 0 0 0 = Just [0]
complexSolveCubic a b 0 0 = Just [0, fromRational $ (-b) / a]
complexSolveCubic a b c 0
  | Just xs <- complexSolveQuadratic a b c = Just $ nub $ 0 : xs
  | otherwise = Just [0]
complexSolveCubic a b c d = map restore <$> complexSolveDepressedCubic p q
  where
    restore t = t - fromRational (b / (3 * a))
    p = (3 * a * c - b * b) / (3 * a * a)
    q = (2 * b * b * b - 9 * a * b * c + 27 * a * a * d) / (27 * a * a * a)

complexSolveDepressedCubic :: Rational -> Rational -> Maybe [Expression]
complexSolveDepressedCubic p q
  | discriminant == 0, p == 0 = Just [0]
  | discriminant == 0 = Just $ map fromRational $ nub [3 * q / p, (-3) / 2 * q / p]
  | p == 0 = Just [x * e | let x = fromRational (-q) ** (1 / 3), e <- [1, e1, e2]]
  | otherwise =
      Just
        [ c - fromRational p / (3 * c),
          c * e1 - fromRational p / (3 * c * e1),
          c * e2 - fromRational p / (3 * c * e2)
        ]
  where
    discriminant = -(4 * p * p * p + 27 * q * q)
    c = (fromRational (-(q / 2)) + sqrt (fromRational (q * q / 4 + p * p * p / 27))) ** (1 / 3)
    e1 = (-1 + sqrt (-3)) / 2
    e2 = (-1 - sqrt (-3)) / 2

complexSolveQuartic :: Rational -> Rational -> Rational -> Rational -> Rational -> Maybe [Expression]
complexSolveQuartic _ 0 0 0 0 = Just [0]
complexSolveQuartic a b 0 0 0 = Just $ nub [0, fromRational $ -(b / a)]
complexSolveQuartic a b c 0 0
  | Just xs <- complexSolveQuadratic a b c = Just $ nub $ 0 : xs
  | otherwise = Just [0]
complexSolveQuartic a b c d 0
  | Just xs <- complexSolveCubic a b c d = Just $ nub $ 0 : xs
  | otherwise = Just [0]
complexSolveQuartic a 0 b 0 c = concatMap restore <$> complexSolveQuadratic a b c
  where
    restore 0 = [0]
    restore x = [sqrt x, -sqrt x]
complexSolveQuartic a b c d e = map restore <$> complexSolveDepressedQuartic p q r
  where
    restore x = x - fromRational (b / (4 * a))
    p = (-3) * b ^ two / (8 * a ^ two) + c / a
    q = b ^ three / (8 * a ^ three) - b * c / (2 * a ^ two) + d / a
    r = (-3) * b ^ four / (256 * a ^ four) + c * b ^ two / (16 * a ^ three) - b * d / (4 * a ^ two) + e / a
    two = 2 :: Int
    three = 3 :: Int
    four = 4 :: Int

complexSolveDepressedQuartic :: Rational -> Rational -> Rational -> Maybe [Expression]
complexSolveDepressedQuartic a 0 c = concatMap restore <$> complexSolveQuadratic 1 a c
  where
    restore 0 = [0]
    restore x = [sqrt x, -sqrt x]
complexSolveDepressedQuartic a b c = do
  ys <- complexSolveCubic 2 (-a) (-(2 * c)) (a * c - b * b / 4)
  y <- case ys of x : _ -> Just x; [] -> Nothing
  let s = sqrt $ 2 * y - fromRational a
  let t = (-2) * y - fromRational a
  return
    [ (1 / 2) * (-s + sqrt (t + 2 * fromRational b / s)),
      (1 / 2) * (-s - sqrt (t + 2 * fromRational b / s)),
      (1 / 2) * (s + sqrt (t - 2 * fromRational b / s)),
      (1 / 2) * (s - sqrt (t - 2 * fromRational b / s))
    ]
