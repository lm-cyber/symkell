module Symkell.Integration.Parts (integrate) where

import Control.Applicative (asum, (<|>))
import Data.Text (Text)
import Symkell.Differentiation
import Symkell.Symbolic
import Symkell.Symbolic.Simplify

integrate ::
  [Text -> Expression -> Maybe Expression] ->
  Text ->
  Expression ->
  Maybe Expression
integrate fs v (x :*: y) = integrate' fs v x y <|> integrate' fs v y x
integrate _ _ _ = Nothing

integrate' ::
  [Text -> Expression -> Maybe Expression] ->
  Text ->
  Expression ->
  Expression ->
  Maybe Expression
integrate' fs v x y = do
  ix <- integrate'' x
  iixdy <- integrate'' $ simplifyForVariable v $ ix * differentiate v y
  return $ ix * y - iixdy
  where
    integrate'' z = asum $ map (\f -> f v z) fs
