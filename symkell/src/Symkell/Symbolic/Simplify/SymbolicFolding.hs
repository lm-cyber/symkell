module Symkell.Symbolic.Simplify.SymbolicFolding (simplify) where

import Symkell.Symbolic

simplify :: Expression -> Expression
simplify e@(Number _) = e
simplify e@(Symbol _) = e
simplify (UnaryApply func x) = unary $ UnaryApply func $ simplify x
simplify (BinaryApply func x y) = binary $ BinaryApply func (simplify x) (simplify y)

unary :: Expression -> Expression
unary (Negate' (Negate' x)) = x
unary (Negate' x) = (-1) * x
unary e = e

binary :: Expression -> Expression
binary e@(x :+: Negate' y)
  | x == y = Number 0
  | otherwise = e
binary e@(Negate' x :+: y)
  | x == y = Number 0
  | otherwise = e
binary (Number 0 :+: x) = x
binary (x :+: Number 0) = x
binary e@((Number n :*: x) :+: ((Number m :*: y) :+: z))
  | x == y = (Number (m + n) :*: x) :+: z
  | otherwise = e
binary e@((Number n :*: x) :+: (y :+: z))
  | x == y = (Number (n + 1) :*: x) :+: z
  | otherwise = e
binary e@(x :+: ((Number n :*: y) :+: z))
  | x == y = (Number (n + 1) :*: x) :+: z
  | otherwise = e
binary e@((Number n :*: x) :+: (Number m :*: y))
  | x == y = Number (n + m) :*: x
  | otherwise = e
binary e@(x :+: (Number n :*: y))
  | x == y = Number (n + 1) :*: x
  | otherwise = e
binary e@((Number n :*: x) :+: y)
  | x == y = Number (n + 1) :*: x
  | otherwise = e
binary e@(x :+: (y :+: z))
  | x == y = Number 2 :*: x :+: z
  | otherwise = e
binary e@(x :+: y)
  | x == y = Number 2 :*: x
  | otherwise = e
binary (Number 0 :*: _) = Number 0
binary (_ :*: Number 0) = Number 0
binary (x :*: Number 1) = x
binary (Number 1 :*: x) = x
binary e@(x :*: (y :**: Number n))
  | x == y = x :**: Number (n + 1)
  | otherwise = e
binary e@((x :**: y) :*: (x' :**: y'))
  | x == x' = x :**: (y :+: y')
  | otherwise = e
binary e@(x :*: ((y :**: Number n) :*: z))
  | x == y = (x :**: Number (n + 1)) :*: z
  | otherwise = e
binary e@((x :**: Number n) :*: (y :*: z))
  | x == y = (x :**: Number (n + 1)) :*: z
  | otherwise = e
binary e@(x :*: (y :*: z))
  | x == y = (x :**: Number 2) :*: z
  | otherwise = e
binary e@(x :*: y)
  | x == y = x :**: Number 2
  | otherwise = e
binary (x :/: (y :/: z)) = (x :*: z) :/: y
binary ((x :/: y) :/: z) = x :/: (y :*: z)
binary (x :/: Number 1) = x
binary (x :/: Number (-1)) = (-1) * x
binary (_ :**: Number 0) = Number 1
binary (1 :**: _) = Number 1
binary (x :**: Number 1) = x
binary (Negate' x :**: Number n)
  | even n = x :**: Number n
  | otherwise = Negate' (x :**: Number n)
binary ((Number (-1) :*: x) :**: Number n)
  | even n = x :**: Number n
  | otherwise = Number (-1) :*: (x :**: Number n)
binary ((x :**: y) :**: z) = x :**: (y :*: z)
binary e@(x :-: y)
  | x == y = Number 0
  | otherwise = e
binary e = e
