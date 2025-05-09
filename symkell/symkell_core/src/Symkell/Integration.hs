module Symkell.Integration (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symkell.Integration.Exponential qualified as Exponential
import Symkell.Integration.Parts qualified as Parts
import Symkell.Integration.Powers qualified as Powers
import Symkell.Integration.Rational qualified as Rational
import Symkell.Integration.Substitution qualified as Substitution
import Symkell.Integration.Sum qualified as Sum
import Symkell.Integration.Term qualified as Term
import Symkell.Integration.Trigonometric qualified as Trigonometric
import Symkell.Symbolic
import Symkell.Symbolic.Simplify

integrate :: Text -> Expression -> Maybe Expression
integrate v e = asum $ map (\f -> f v e') withTermSum
  where
    e' = simplifyForVariable v e

base :: [Text -> Expression -> Maybe Expression]
base = [Powers.integrate, Exponential.integrate, Trigonometric.integrate, Rational.integrate]

withTerm :: [Text -> Expression -> Maybe Expression]
withTerm =
  base
    ++ [ Term.integrate base,
         Substitution.integrate base,
         Parts.integrate [Term.integrate base],
         Term.integrate [Substitution.integrate base, Parts.integrate [Term.integrate base]]
       ]

withTermSum :: [Text -> Expression -> Maybe Expression]
withTermSum = withTerm ++ [Sum.integrate withTerm]
