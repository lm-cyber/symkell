module Symkell.Limit (limit) where

import Data.Text (Text)
import Symkell.Symbolic
import Symkell.Symbolic.Simplify (simplify, tidy)
import Symkell.Differentiation (differentiate)
import Data.Maybe (isJust)

-- Computes the limit of an expression as a variable approaches a target value.
-- Tries direct substitution, simplification, term-wise limits, and L'Hopital's rule for 0/0.
-- Still lacks handling for infinity and most indeterminate forms beyond 0/0.
limit :: Text -> Expression -> Expression -> Maybe Expression
limit var target expr = limit' var target (tidy $ simplify expr)

limit' :: Text -> Expression -> Expression -> Maybe Expression
-- Base Cases
limit' _ _ e@(Number _) = Just e -- Limit of a constant is the constant
limit' var target e@(Symbol s)
  | s == var = Just target -- Limit of x as x->a is a
  | otherwise = Just e -- Limit of another symbol is the symbol itself

-- Recursive Cases (Term-wise Limits)
limit' var target (UnaryApply func x) =
  getUnaryFunction func <$> limit' var target x -- Apply function to the limit of the argument

limit' var target (x :+: y) = (+) <$> limit' var target x <*> limit' var target y
limit' var target (x :*: y) = (*) <$> limit' var target x <*> limit' var target y
limit' var target (x :-: y) = (-) <$> limit' var target x <*> limit' var target y
limit' var target (x :**: y) = (**) <$> limit' var target x <*> limit' var target y -- Be careful with 0^0, inf^0, 1^inf
limit' var target (LogBase' x y) = logBase <$> limit' var target x <*> limit' var target y

-- Fractions and L'Hopital's Rule
limit' var target (num :/: den) =
  let limitNumMaybe = limit' var target num
      limitDenMaybe = limit' var target den
  in case (limitNumMaybe, limitDenMaybe) of
       -- Check for 0/0 form
       (Just (Number 0), Just (Number 0)) ->
         applyLHopital var target num den -- Attempt L'Hopital's rule

       -- Check for c/0 form (would be +/- infinity, return Nothing for now)
       (Just n, Just (Number 0)) | not (isZeroExpr n) -> Nothing

       -- If denominator limit is non-zero, compute the limit of the fraction
       (Just n, Just d) | not (isZeroExpr d) -> Just $ simplify (n / d)

       -- Any other case (e.g., limits couldn't be determined, involves symbols)
       _ -> Nothing -- Cannot determine limit in other cases yet

-- L'Hopital's Rule Application (called when 0/0 is detected)
applyLHopital :: Text -> Expression -> Expression -> Expression -> Maybe Expression
applyLHopital var target num den =
  let dNum = simplify $ differentiate var num
      dDen = simplify $ differentiate var den
      -- Check the limit of the derivative of the denominator
      limitDDenMaybe = limit' var target dDen
  in case limitDDenMaybe of
       Just d | not (isZeroExpr d) -> -- If limit of denominator derivative is non-zero
          limit' var target (dNum :/: dDen) -- Recursively compute limit of dNum/dDen
       _ -> Nothing -- L'Hopital's rule fails if limit of denominator derivative is zero or undetermined

-- Helper Functions
isZeroExpr :: Expression -> Bool
isZeroExpr (Number 0) = True
isZeroExpr _ = False

-- TODO:
-- - Add Representation & Handling for Infinity (+/-, unsigned)
-- - Handle Indeterminate Forms: oo/oo, 0*oo, oo-oo, 1^oo, 0^0, oo^0
-- - Variable Transformations (z=1/x for oo, z=z0+x for z0)
-- - Bidirectional limits (dir='+-' or specific '+' or '-')
-- - Series Expansion (leadterm/Gruntz) - Major undertaking
-- - More robust simplification steps.

-- NOTE: This is a very basic implementation.
-- Needs significant enhancement for:
-- * Handling Infinity: The Expression type doesn't represent infinity.
-- * Other Indeterminate Forms: infinity/infinity, 0*infinity, infinity-infinity, 1^infinity, 0^0, infinity^0.
-- * More Robust L'Hopital: Handling repeated applications, cases where derivatives are complex.
-- * One-Sided Limits: Necessary for functions with discontinuities.
-- * Discontinuities: Correctly identifying and handling jumps, holes, asymptotes.
-- * Symbolic Targets: Limit as x approaches 'a' symbolically.
-- * More sophisticated simplification strategies. 