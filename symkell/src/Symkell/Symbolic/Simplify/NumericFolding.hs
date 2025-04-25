module Symkell.Symbolic.Simplify.NumericFolding (simplify) where

import Symkell.Numeric (root)
import Symkell.Symbolic

simplify :: Expression -> Expression
simplify e@(Number _) = e
simplify e@(Symbol _) = e
simplify (UnaryApply func x) = unary $ UnaryApply func $ simplify x
simplify (BinaryApply func x y) = binary $ BinaryApply func (simplify x) (simplify y)

unary :: Expression -> Expression
unary (Negate' (Number n)) = Number (-n)
unary (Negate' (Number n :/: Number m))
  | m < 0 = simplify $ Number n :/: Number (-m)
  | otherwise = simplify $ Number (-n) :/: Number m
unary (Abs' (Number n)) = Number $ abs n
unary (Signum' (Number n)) = Number $ signum n
unary (Exp' x) = simplifyExp x
unary (Log' x) = simplifyLog x
unary (Sqrt' x) = simplify $ x :**: (Number 1 :/: Number 2)
unary (Sin' x) = simplifySin x
unary (Cos' x) = simplifyCos x
unary (Tan' x) = simplifyTan x
unary e = e

binary :: Expression -> Expression
binary (Number 0 :+: x) = x
binary (x :+: Number 0) = x
binary (Number n :+: Number m) = Number (n + m)
binary ((Number n :/: Number m) :+: Number k) = reduceRatio (n + m * k) m
binary (Number n :+: (Number m :/: Number k)) = reduceRatio (n * k + m) k
binary ((Number n :/: Number m) :+: (Number k :/: Number l)) = reduceRatio (n * l + k * m) (m * l)
binary ((x :+: Number n) :+: Number m) = Number (n + m) :+: x
binary ((Number n :+: x) :+: Number m) = Number (n + m) :+: x
binary (Number n :+: (x :+: Number m)) = Number (n + m) :+: x
binary (Number n :+: (Number m :+: x)) = Number (n + m) :+: x
binary (Number 0 :*: _) = Number 0
binary (_ :*: Number 0) = Number 0
binary (Number 1 :*: x) = x
binary (x :*: Number 1) = x
binary (Number n :*: Number m) = Number (n * m)
binary (Number n :*: (Number m :/: Number k)) = reduceRatio (n * m) k
binary ((Number n :/: Number m) :*: Number k) = reduceRatio (n * k) m
binary ((Number n :/: Number m) :*: (Number k :/: Number l)) = reduceRatio (n * k) (m * l)
binary ((x :*: Number n) :*: Number m) = Number (n * m) :*: x
binary ((Number n :*: x) :*: Number m) = Number (n * m) :*: x
binary (Number n :*: (x :*: Number m)) = Number (n * m) :*: x
binary (Number n :*: (Number m :*: x)) = Number (n * m) :*: x
binary e@(Number n :*: (x :/: Number m)) | m /= 0, m == n = x | otherwise = e
binary e@((x :/: Number n) :*: Number m) | n /= 0, m == n = x | otherwise = e
binary (x@(Number _) :*: (y@(Number _ :/: Number _) :*: z)) = simplify (x :*: y) :*: z
binary (x@(Number _ :/: Number _) :*: (y@(Number _ :/: Number _) :*: z)) = simplify (x :*: y) :*: z
binary (x :-: y) = simplify $ x :+: Negate' y
binary e@(_ :/: (_ :/: 0)) = e
binary (x :/: (y :/: z)) = simplify $ (x :*: z) :/: y
binary e@((_ :/: 0) :/: _) = e
binary e@((_ :/: _) :/: 0) = e
binary ((x :/: y) :/: z) = simplify $ x :/: (y :*: z)
binary (Number n :/: Number m) = reduceRatio n m
binary e@(Number 0 :**: Number 0) = e
binary (Number _ :**: Number 0) = Number 1
binary (Number 1 :**: _) = Number 1
binary (Number n :**: Number m)
  | m >= 0 = Number (n ^ m)
  | otherwise = Number 1 :/: Number (n ^ (-m))
binary ((Number n :/: Number m) :**: Number k)
  | k >= 0 = Number (n ^ k) :/: Number (m ^ k)
  | otherwise = Number (m ^ (-k)) :/: Number (n ^ (-k))
binary e@(Number n :**: c@(Number m :/: Number k))
  | (Just l) <- root n k, m >= 0 = Number (l ^ m)
  | (Just l) <- root n k, m < 0 = 1 :/: Number (l ^ (-m))
  | n < 0, n /= -1, even k = (-1) ** c * simplify (Number (-n) ** c)
  | otherwise = e
binary e@((Number n :/: Number m) :**: (Number k :/: Number l))
  | (Just n', Just m') <- (root n l, root m l) = (Number n' :/: Number m') :**: Number k
  | otherwise = e
binary (LogBase' b x) = simplify $ Log' x :/: Log' b
binary e = e

reduceRatio :: Integer -> Integer -> Expression
reduceRatio n 0 = Number n :/: Number 0
reduceRatio n 1 = Number n
reduceRatio n m
  | m == d = Number (n `div` m)
  | m == -d = Number (n `div` m)
  | n < 0, m < 0 = Number (-(n `div` d)) :/: Number (-(m `div` d))
  | otherwise = Number (n `div` d) :/: Number (m `div` d)
  where
    d = gcd n m

simplifyExp :: Expression -> Expression
simplifyExp (Number 0) = Number 1
simplifyExp (Log' x) = x
simplifyExp e = Exp' e

simplifyLog :: Expression -> Expression
simplifyLog (Number 1) = Number 0
simplifyLog (Exp' x) = x
simplifyLog e = Log' e

simplifySin :: Expression -> Expression
simplifySin (Number 0) = 0
simplifySin (Number _ :*: Pi') = 0
simplifySin (Pi' :*: Number _) = 0
simplifySin ((Number n :/: 2) :*: Pi')
  | even n = 0
  | odd ((n - 1) `div` 2) = 1
  | otherwise = -1
simplifySin (Pi' :*: (Number n :/: 2))
  | even n = 0
  | odd ((n - 1) `div` 2) = 1
  | otherwise = -1
simplifySin e = Sin' e

simplifyCos :: Expression -> Expression
simplifyCos (Number 0) = 1
simplifyCos (Number n :*: Pi') | even n = 1 | odd n = -1
simplifyCos (Pi' :*: Number n) | even n = 1 | odd n = -1
simplifyCos ((Number _ :/: 2) :*: Pi') = 0
simplifyCos (Pi' :*: (Number _ :/: 2)) = 0
simplifyCos e = Cos' e

simplifyTan :: Expression -> Expression
simplifyTan (Number 0) = 0
simplifyTan e = Tan' e
