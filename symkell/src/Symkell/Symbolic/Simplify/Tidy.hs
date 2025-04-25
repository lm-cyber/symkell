module Symkell.Symbolic.Simplify.Tidy (tidy) where

import Symkell.Symbolic

tidy :: Expression -> Expression
tidy (UnaryApply func x) = unary $ UnaryApply func $ tidy x
tidy (BinaryApply func x y) = binary $ BinaryApply func (tidy x) (tidy y)
tidy e = e

unary :: Expression -> Expression
unary e = e

binary :: Expression -> Expression
binary (x :+: (Negate' y)) = x :-: y
binary (Number (-1) :*: x) = Negate' x
binary e@(Number n :*: x)
  | n < 0 = Negate' (Number (-n) :*: x)
  | otherwise = e
binary e@(Number n :/: x)
  | n < 0 = Negate' (Number (-n) :/: x)
  | otherwise = e
binary (Negate' x :*: Negate' y) = x :*: y
binary (Negate' x :*: y) = Negate' $ x :*: y
binary (x :*: Negate' y) = Negate' $ x :*: y
binary (x :+: (Negate' y :+: z)) = (x :-: y) :+: z
binary (x :**: (Number 1 :/: Number 2)) = sqrt x
binary e = e
