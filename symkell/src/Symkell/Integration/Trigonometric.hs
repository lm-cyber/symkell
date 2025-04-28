module Symkell.Integration.Trigonometric (integrate) where

import Data.Text (Text)
import Symkell.Symbolic

integrate :: Text -> Expression -> Maybe Expression
integrate _ (Number _) = Nothing
integrate _ (Symbol _) = Nothing
integrate v (Sin' x@(Symbol s))
  | s == v = Just $ Negate' $ Cos' x
  | otherwise = Nothing
integrate v (Cos' x@(Symbol s))
  | s == v = Just $ Sin' x
  | otherwise = Nothing
integrate v (Tan' x@(Symbol s))
  | s == v = Just $ Negate' $ Log' $ Abs' $ Cos' x
  | otherwise = Nothing
integrate v (Asin' x@(Symbol s))
  | s == v = Just $ (x :*: Asin' x) :+: Sqrt' (1 :-: (x :**: 2))
  | otherwise = Nothing
integrate v (Acos' x@(Symbol s))
  | s == v = Just $ (x :*: Acos' x) :-: Sqrt' (1 :-: (x :**: 2))
  | otherwise = Nothing
integrate v (Atan' x@(Symbol s))
  | s == v = Just $ (x :*: Atan' x) :-: (Log' ((x :**: 2) :+: 1) :/: 2)
  | otherwise = Nothing
integrate v (Sinh' x@(Symbol s))
  | s == v = Just $ Cosh' x
  | otherwise = Nothing
integrate v (Cosh' x@(Symbol s))
  | s == v = Just $ Sinh' x
  | otherwise = Nothing
integrate v (Tanh' x@(Symbol s))
  | s == v = Just $ Log' $ Cosh' x
  | otherwise = Nothing
integrate v (Asinh' x@(Symbol s))
  | s == v = Just $ (x :*: Asinh' x) :-: Sqrt' ((x :**: 2) + 1)
  | otherwise = Nothing
integrate v (Acosh' x@(Symbol s))
  | s == v = Just $ (x :*: Acosh' x) :-: (Sqrt' (x :+: 1) :*: Sqrt' (x :-: 1))
  | otherwise = Nothing
integrate v (Atanh' x@(Symbol s))
  | s == v = Just $ (x :*: Atanh' x) :+: (Log' (1 :-: (x :**: 2)) :/: 2)
  | otherwise = Nothing
integrate _ _ = Nothing
