module Symkell.Polynomial.Symbolic
  ( fromExpression,
    forVariable,
    withSymbolicCoefficients,
    toExpression,
    toRationalCoefficient,
    toSymbolicCoefficient,
  )
where

import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Symkell.Polynomial
import Symkell.Symbolic

fromExpression ::
  (Polynomial p e c, Num (p e c), Fractional c) =>
  (Text -> Maybe (p e c), Expression -> Maybe c) ->
  Expression ->
  Maybe (p e c)
fromExpression _ (Number n) = Just $ fromInteger n
fromExpression (cf, _) (Symbol x) = cf x
fromExpression t (Negate' x) = negate <$> fromExpression t x
fromExpression t (x :+: y) = (+) <$> fromExpression t x <*> fromExpression t y
fromExpression t (x :*: y) = (*) <$> fromExpression t x <*> fromExpression t y
fromExpression t (x :-: y) = (-) <$> fromExpression t x <*> fromExpression t y
fromExpression t (x :**: (Number n))
  | n >= 0 = (^ n) <$> fromExpression t x
  | otherwise = Nothing
fromExpression _ (_ :**: _) = Nothing
fromExpression _ (_ :/: Number 0) = Nothing
fromExpression _ (Number n :/: Number m) = Just $ scale r 1
  where
    r = fromInteger n / fromInteger m
fromExpression (_, eval) e
  | Just e' <- eval e = Just $ scale e' 1
  | otherwise = Nothing

forVariable ::
  (Polynomial p e c, Num (p e c), Fractional c) =>
  Text ->
  (Text -> Maybe (p e c), Expression -> Maybe c)
forVariable v = (fromSymbol, toCoefficient)
  where
    fromSymbol s
      | v == s = Just $ power 1
      | otherwise = Nothing
    toCoefficient (Symbol _) = Nothing
    toCoefficient (Number n) = Just $ fromInteger n
    toCoefficient (Negate' x) = negate <$> toCoefficient x
    toCoefficient (Abs' x) = abs <$> toCoefficient x
    toCoefficient (Signum' x) = signum <$> toCoefficient x
    toCoefficient (x :+: y) = (+) <$> toCoefficient x <*> toCoefficient y
    toCoefficient (x :*: y) = (*) <$> toCoefficient x <*> toCoefficient y
    toCoefficient (x :-: y) = (-) <$> toCoefficient x <*> toCoefficient y
    toCoefficient (_ :/: 0) = Nothing
    toCoefficient (x :/: y) = (/) <$> toCoefficient x <*> toCoefficient y
    toCoefficient (x :**: (Number n)) = (^^ n) <$> toCoefficient x
    toCoefficient _ = Nothing

withSymbolicCoefficients ::
  (Polynomial p e Expression, Num (p e Expression), Integral e) =>
  (Text -> Maybe (p e Expression), Expression -> Maybe Expression) ->
  (Text -> Maybe (p e Expression), Expression -> Maybe Expression)
withSymbolicCoefficients (fromSymbol, _) = (fromSymbol', toCoefficient)
  where
    fromSymbol' s = Just $ fromMaybe (scale (Symbol s) 1) (fromSymbol s)
    toCoefficient e@(Symbol s)
      | Nothing <- fromSymbol s = Just e
      | otherwise = Nothing
    toCoefficient e@(Number _) = Just e
    toCoefficient (UnaryApply func x) = UnaryApply func <$> toCoefficient x
    toCoefficient (BinaryApply func x y) = BinaryApply func <$> x' <*> y'
      where
        x' = toCoefficient x
        y' = toCoefficient y

toExpression :: (Polynomial p e c) => Text -> (c -> Expression) -> p e c -> Expression
toExpression x cf p = getSum $ foldTerms convert p
  where
    convert 0 c = Sum $ cf c
    convert e c = Sum $ cf c * xp
      where
        xp = Symbol x ** Number (fromIntegral e)

toRationalCoefficient :: (Real c) => c -> Expression
toRationalCoefficient c
  | d == 1 = Number n
  | otherwise = Number n :/: Number d
  where
    r = toRational c
    n = fromInteger $ numerator r
    d = fromInteger $ denominator r

toSymbolicCoefficient :: Expression -> Expression
toSymbolicCoefficient = id
