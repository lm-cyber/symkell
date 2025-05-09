{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

module Symkell.Polynomial.Rational
  ( Function,
    fromPolynomial,
    fromPolynomials,
    toPolynomial,
    pattern Function,
  )
where

import Control.DeepSeq (NFData)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Symkell.Polynomial
import TextShow

data Function a = F a a deriving (Generic, NFData)

pattern Function :: a -> a -> Function a
pattern Function x y <- F x y

instance (Show a, TextShow a) => Show (Function a) where
  show = unpack . showt

instance (TextShow a) => TextShow (Function a) where
  showb (F x y) = "Function (" <> showb x <> ") (" <> showb y <> ")"

instance (Eq a, Num a) => Eq (Function a) where
  (F x 0) == (F y 0) = x == y
  (F x y) == (F u v) = x * v == y * u

instance (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) => Num (Function (p e c)) where
  (F x y) + (F u v) = fromPolynomials (x * v + u * y) (y * v)
  (F x y) - (F u v) = fromPolynomials (x * v - u * y) (y * v)
  (F x y) * (F u v) = fromPolynomials (x * u) (y * v)
  abs = id
  signum = const 1
  fromInteger n = fromPolynomial $ fromInteger n

instance (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) => Fractional (Function (p e c)) where
  (F x y) / (F u v) = fromPolynomials (x * v) (y * u)
  fromRational x = fromPolynomial $ scale (fromRational x) 1

fromPolynomial :: (Polynomial p e c, Num (p e c)) => p e c -> Function (p e c)
fromPolynomial x = F x 1

fromPolynomials ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  p e c ->
  p e c ->
  Function (p e c)
fromPolynomials x y = F x'' y''
  where
    g = greatestCommonDivisor x y
    (x', _) = x `divide` g
    (y', _) = y `divide` g
    (x'', y'')
      | 0 <- y' = (x', y')
      | 1 <- y' = (x', y')
      | otherwise = (scale (1 / leadingCoefficient y') x', scale (1 / leadingCoefficient y') y')

toPolynomial :: (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) => Function (p e c) -> Maybe (p e c)
toPolynomial (F x 1) = Just x
toPolynomial (F x y)
  | 0 <- degree y, y /= 0 = Just $ scale (1 / leadingCoefficient y) x
  | otherwise = Nothing
