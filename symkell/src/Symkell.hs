module Symkell
  ( Expression,
    integrate,
    differentiate,
    evaluate,
    fractionalEvaluate,
    toFunction,
    toHaskell,
    toLaTeX,
    simplify,
    tidy,
  )
where

import Data.Text (Text)
import Symkell.Differentiation (differentiate)
import Symkell.Integration qualified as Integration
import Symkell.Symbolic (Expression, evaluate, fractionalEvaluate, toFunction)
import Symkell.Symbolic.Haskell (toHaskell)
import Symkell.Symbolic.LaTeX (toLaTeX)
import Symkell.Symbolic.Simplify (simplify, simplifyForVariable)
import Symkell.Symbolic.Simplify.Tidy (tidy)
import Symkell.Limit (limit)

integrate ::
  Text ->
  Expression ->
  Maybe Expression
integrate var expr = tidy . simplifyForVariable var <$> Integration.integrate var expr
