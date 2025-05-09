module Symkell.Integration.Exponential (integrate) where

import Data.Text (Text)
import Symkell.Symbolic

integrate :: Text -> Expression -> Maybe Expression
integrate _ (Number _) = Nothing
integrate _ (Symbol _) = Nothing
integrate v e@(Exp' (Symbol s))
  | v == s = Just e
  | otherwise = Nothing
integrate v (Log' e@(Symbol s))
  | v == s = Just $ (e :*: Log' e) :-: e
  | otherwise = Nothing
integrate v e@(Number n :**: Symbol s)
  | v == s = Just $ c :*: e
  | otherwise = Nothing
  where
    c = Number 1 :/: Log' (Number n)
integrate v (LogBase' (Number n) (Symbol s))
  | v == s = fmap (\x -> x :/: Log' (Number n)) $ integrate v $ Log' (Symbol s)
integrate _ _ = Nothing
