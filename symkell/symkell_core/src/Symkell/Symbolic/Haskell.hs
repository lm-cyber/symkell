module Symkell.Symbolic.Haskell
  ( toHaskell,
    getUnaryFunctionText,
    getBinaryFunctionText,
  )
where

import Data.Text
import Symkell.Symbolic
import TextShow (showt)

toHaskell :: Expression -> Text
toHaskell (Number n) = showt n
toHaskell (Symbol t) = t
toHaskell (UnaryApply fun x) = funcText <> " " <> asArg x
  where
    funcText = getUnaryFunctionText fun
toHaskell (LogBase' x y) = funcText <> " " <> asArg x <> " " <> asArg y
  where
    funcText = getBinaryFunctionText LogBase
toHaskell (x :+: y) = asAddArg x <> " + " <> asAddArg y
toHaskell (x :-: y@(_ :+: _)) = asAddArg x <> " - " <> asArg y
toHaskell (x :-: y@(_ :-: _)) = asAddArg x <> " - " <> asArg y
toHaskell (x :-: y) = asAddArg x <> " - " <> asAddArg y
toHaskell (x :*: y) = asMultiplyArg x <> " * " <> asMultiplyArg y
toHaskell (BinaryApply op x y) = asArg x <> " " <> opText <> " " <> asArg y
  where
    opText = getBinaryFunctionText op

asArg :: Expression -> Text
asArg x@(Number n)
  | n >= 0 = toHaskell x
  | otherwise = "(" <> toHaskell x <> ")"
asArg x@(Symbol _) = toHaskell x
asArg x = par $ toHaskell x

asAddArg :: Expression -> Text
asAddArg x@(Number _) = asArg x
asAddArg x@(Symbol _) = asArg x
asAddArg x = toHaskell x

asMultiplyArg :: Expression -> Text
asMultiplyArg x@(Number _) = asArg x
asMultiplyArg x@(Symbol _) = asArg x
asMultiplyArg x@(_ :+: _) = par $ toHaskell x
asMultiplyArg x@(_ :-: _) = par $ toHaskell x
asMultiplyArg x = toHaskell x

par :: Text -> Text
par s = "(" <> s <> ")"

getUnaryFunctionText :: UnaryFunction -> Text
getUnaryFunctionText Negate = "negate"
getUnaryFunctionText Abs = "abs"
getUnaryFunctionText Signum = "signum"
getUnaryFunctionText Exp = "exp"
getUnaryFunctionText Log = "log"
getUnaryFunctionText Sqrt = "sqrt"
getUnaryFunctionText Sin = "sin"
getUnaryFunctionText Cos = "cos"
getUnaryFunctionText Tan = "tan"
getUnaryFunctionText Asin = "asin"
getUnaryFunctionText Acos = "acos"
getUnaryFunctionText Atan = "atan"
getUnaryFunctionText Sinh = "sinh"
getUnaryFunctionText Cosh = "cosh"
getUnaryFunctionText Tanh = "tanh"
getUnaryFunctionText Asinh = "asinh"
getUnaryFunctionText Acosh = "acosh"
getUnaryFunctionText Atanh = "atanh"

getBinaryFunctionText :: BinaryFunction -> Text
getBinaryFunctionText Add = "+"
getBinaryFunctionText Multiply = "*"
getBinaryFunctionText Subtract = "-"
getBinaryFunctionText Divide = "/"
getBinaryFunctionText Power = "**"
getBinaryFunctionText LogBase = "logBase"
