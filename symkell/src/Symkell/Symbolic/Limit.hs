{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Symkell.Symbolic.Limit
  ( limit,
    LimitPoint (..),
    Direction (..),
    ExtendedReal (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Maybe (isJust, fromMaybe)
import Data.Ratio (Ratio, numerator, denominator, (%))
import Data.Text (Text)
import GHC.Generics (Generic)
import Symkell.Symbolic
import Symkell.Symbolic.Simplify (simplify, simplifyForVariable)
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))

-- | Represents the point towards which the limit is taken.
data LimitPoint
  = PositiveInfinity
  | NegativeInfinity
  | RealValue Rational -- Using Rational for exact representation
  deriving (Eq, Show, Read, Generic, NFData)
  deriving (TextShow) via FromGeneric LimitPoint

-- | Represents the direction from which the limit is approached.
data Direction
  = FromLeft
  | FromRight
  | Bidirectional
  deriving (Eq, Show, Read, Generic, NFData)
  deriving (TextShow) via FromGeneric Direction

-- | Represents extended real numbers, including infinities and undefined results.
data ExtendedReal
  = NegInfinity
  | PosInfinity
  | Finite Rational
  | Undefined -- For cases where the limit does not exist
  | Indeterminate -- For indeterminate forms like 0/0 before resolution
  deriving (Eq, Show, Read, Generic, NFData)
  deriving (TextShow) via FromGeneric ExtendedReal

-- | Computes the limit of an expression.
limit :: Expression -> Text -> LimitPoint -> Direction -> ExtendedReal
limit expr var limitPoint Bidirectional =
  case (limit expr var limitPoint FromLeft, limit expr var limitPoint FromRight) of
    (Finite a, Finite b) | a == b -> Finite a
    (PosInfinity, PosInfinity) -> PosInfinity
    (NegInfinity, NegInfinity) -> NegInfinity
    _ -> Undefined -- Left and right limits aren't equal

limit expr var limitPoint direction =
  let 
    -- First simplify the expression
    simplifiedExpr = simplifyForVariable var expr
  in
    -- Apply transformations based on the limit point
    case limitPoint of
      RealValue x -> 
        -- For a real value, try direct substitution first
        case directSubstitution simplifiedExpr var x of
          Just result -> result
          Nothing -> 
            -- If direct substitution fails, transform x to z0 + h and compute limit as h→0
            let transformed = transformToFinitePoint simplifiedExpr var x
                dir = case direction of
                       FromLeft -> FromLeft
                       FromRight -> FromRight
                       Bidirectional -> Bidirectional
            in computeLimitAtZero transformed "h" dir
            
      PositiveInfinity ->
        -- For limit at +∞, substitute x = 1/h and compute limit as h→0+
        let transformed = transformToInfinity simplifiedExpr var False
        in computeLimitAtZero transformed "h" FromRight
        
      NegativeInfinity ->
        -- For limit at -∞, substitute x = -1/h and compute limit as h→0+
        let transformed = transformToInfinity simplifiedExpr var True
        in computeLimitAtZero transformed "h" FromRight

-- | Tries direct substitution of the value into the expression
directSubstitution :: Expression -> Text -> Rational -> Maybe ExtendedReal
directSubstitution expr var value = 
  -- Simple case: if expr is just the variable, return the value
  case expr of
    Symbol s | s == var -> Just (Finite value)
    _ -> 
      let 
        -- Create a substitution function that replaces var with value
        subst s | s == var = Just (Number (numerator value) :/: Number (denominator value))
               | otherwise = Nothing
        
        -- Substitute and try to evaluate
        substituted = substitute expr subst
        evaluator s | s == var = Just (fromRational value)
                   | otherwise = Nothing
      in
        case evaluate substituted evaluator of
          Just val -> Just (Finite (toRational val))
          Nothing -> Nothing  -- Substitution led to an undefined result

-- | Transforms an expression for limit at infinity
transformToInfinity :: Expression -> Text -> Bool -> Expression
transformToInfinity expr var isNegative =
  let 
    -- Substitute x with 1/h or -1/h
    subst s | s == var = 
              if isNegative 
              then Just (Number (-1) :/: Symbol "h") 
              else Just (Number 1 :/: Symbol "h")
            | otherwise = Nothing
  in
    substitute expr subst

-- | Transforms an expression for limit at a finite point
transformToFinitePoint :: Expression -> Text -> Rational -> Expression
transformToFinitePoint expr var point =
  let 
    -- Substitute x with z0 + h
    z0 = Number (numerator point) :/: Number (denominator point)
    subst s | s == var = Just (z0 :+: Symbol "h")
            | otherwise = Nothing
  in
    substitute expr subst

-- | Computes the limit of an expression as the variable approaches 0
computeLimitAtZero :: Expression -> Text -> Direction -> ExtendedReal
computeLimitAtZero expr var direction =
  let 
    -- Simplify the expression
    simplifiedExpr = simplifyForVariable var expr
  in
    -- Try various methods in order of complexity
    fromMaybe Undefined $ 
      tryDirectEvaluation simplifiedExpr var direction <|>
      tryAlgebraicSimplification simplifiedExpr var direction <|>
      tryLHopitalsRule simplifiedExpr var direction <|>
      trySeriesExpansion simplifiedExpr var direction

-- | Try direct evaluation at 0, handle special cases
tryDirectEvaluation :: Expression -> Text -> Direction -> Maybe ExtendedReal
tryDirectEvaluation expr var _ =
  -- Try direct substitution of 0
  case directSubstitution expr var 0 of
    Just result -> Just result
    Nothing -> 
      -- Handle special cases like simple poles, etc.
      case expr of
        -- For C/x^n, return ±∞ based on sign of C and direction
        BinaryApply Divide (Number c) (BinaryApply Power (Symbol s) (Number n))
          | s == var && n > 0 ->
            if c > 0 
            then Just PosInfinity
            else Just NegInfinity
        
        -- Handle 0/0 forms specially
        _ -> Nothing

-- | Try algebraic simplification for indeterminate forms
tryAlgebraicSimplification :: Expression -> Text -> Direction -> Maybe ExtendedReal
tryAlgebraicSimplification expr var direction =
  -- Handle common indeterminate forms
  case expr of
    -- 0/0 form - try to simplify algebraically
    BinaryApply Divide num den ->
      let 
        subst s | s == var = Just (Number 0)
               | otherwise = Nothing
        
        numAtZero = substitute num subst
        denAtZero = substitute den subst
        
        evaluator s | s == var = Just 0
                   | otherwise = Nothing
      in
        if evaluate numAtZero evaluator == Just 0 && evaluate denAtZero evaluator == Just 0
        then
          -- Try symbolic differentiation for 0/0 form
          let 
            numDiff = differentiate num var
            denDiff = differentiate den var
            newExpr = BinaryApply Divide numDiff denDiff
          in
            Just (fromMaybe Indeterminate (tryDirectEvaluation newExpr var direction))
        else Nothing
    
    -- inf-inf form - try to find common denominator
    BinaryApply Subtract a b ->
      -- Check if both terms go to infinity
      let 
        aLimit = computeLimitAtZero a var direction
        bLimit = computeLimitAtZero b var direction
      in
        if (aLimit == PosInfinity && bLimit == PosInfinity) ||
           (aLimit == NegInfinity && bLimit == NegInfinity)
        then
          -- Try to rewrite as a single fraction
          let
            combined = simplifyForVariable var expr
          in
            if combined /= expr
            then tryDirectEvaluation combined var direction
            else Nothing
        else Nothing
        
    -- 1^∞ form - rewrite as exp(lim[g*(f-1)])
    BinaryApply Power base exponent ->
      let
        baseLimit = computeLimitAtZero base var direction
        exponentLimit = computeLimitAtZero exponent var direction
      in
        if baseLimit == Finite 1 && (exponentLimit == PosInfinity || exponentLimit == NegInfinity)
        then
          let
            newExpr = UnaryApply Exp (BinaryApply Multiply exponent (BinaryApply Subtract base (Number 1)))
          in
            tryDirectEvaluation newExpr var direction
        else Nothing
    
    _ -> Nothing

-- | Try L'Hôpital's rule for indeterminate forms
tryLHopitalsRule :: Expression -> Text -> Direction -> Maybe ExtendedReal
tryLHopitalsRule expr var direction =
  case expr of
    BinaryApply Divide num den ->
      let 
        numLimit = computeLimitAtZero num var direction
        denLimit = computeLimitAtZero den var direction
      in
        if (numLimit == Finite 0 && denLimit == Finite 0) ||
           ((numLimit == PosInfinity || numLimit == NegInfinity) && 
            (denLimit == PosInfinity || denLimit == NegInfinity))
        then
          let 
            numDiff = differentiate' num var
            denDiff = differentiate' den var
            newExpr = BinaryApply Divide numDiff denDiff
          in
            Just (fromMaybe Indeterminate (tryDirectEvaluation newExpr var direction))
        else Nothing
    _ -> Nothing

-- | Try series expansion methods (incomplete - would need more implementation)
trySeriesExpansion :: Expression -> Text -> Direction -> Maybe ExtendedReal
trySeriesExpansion _ _ _ = Nothing

-- | Helper function to differentiate an expression with respect to a variable
-- This is a simplified version - we would need more careful implementation
differentiate' :: Expression -> Text -> Expression
differentiate' expr var =
  case expr of
    Number _ -> Number 0
    Symbol s | s == var -> Number 1
             | otherwise -> Number 0
    UnaryApply Negate x -> UnaryApply Negate (differentiate' x var)
    BinaryApply Add x y -> BinaryApply Add (differentiate' x var) (differentiate' y var)
    BinaryApply Subtract x y -> BinaryApply Subtract (differentiate' x var) (differentiate' y var)
    BinaryApply Multiply x y -> 
      BinaryApply Add 
        (BinaryApply Multiply (differentiate' x var) y)
        (BinaryApply Multiply x (differentiate' y var))
    BinaryApply Divide x y ->
      BinaryApply Divide
        (BinaryApply Subtract
          (BinaryApply Multiply (differentiate' x var) y)
          (BinaryApply Multiply x (differentiate' y var)))
        (BinaryApply Power y (Number 2))
    _ -> Number 0  -- Simplified placeholder

-- | Alternative to Maybe's (<|>) operator
(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> y = y
x <|> _ = x 