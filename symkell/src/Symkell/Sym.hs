{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Symkell.Sym
  (
    Expression (..),
    UnaryFunction (..),
    BinaryFunction (..),
    substitute,
    evaluate,
    fractionalEvaluate,
    toFunction,
    getUnaryFunction,
    getBinaryFunction,
    pattern Pi',
    pattern Negate',
    pattern Abs',
    pattern Signum',
    pattern Exp',
    pattern Log',
    pattern Sqrt',
    pattern Sin',
    pattern Cos',
    pattern Tan',
    pattern Asin',
    pattern Acos',
    pattern Atan',
    pattern Sinh',
    pattern Cosh',
    pattern Tanh',
    pattern Asinh',
    pattern Acosh',
    pattern Atanh',
    pattern (:+:),
    pattern (:*:),
    pattern (:-:),
    pattern (:/:),
    pattern (:**:),
    pattern LogBase',
  )
where

import Control.DeepSeq (NFData)
import Data.Ratio
import Data.String (IsString, fromString)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))

data Expression
  = Number Integer | Symbol Text | UnaryApply UnaryFunction Expression | BinaryApply BinaryFunction Expression Expression
  deriving
    ( 
      Eq,
      Show,
      Read,
      Generic,
      NFData
    )
  deriving (TextShow) via FromGeneric Expression

pattern Pi' :: Expression
pattern Pi' = Symbol "pi"

data UnaryFunction
  = Negate | Abs | Signum | Exp | Log | Sqrt | Sin | Cos | Tan | Asin | Acos | Atan | Sinh | Cosh | Tanh | Asinh | Acosh | Atanh
  deriving (Eq, Enum, Bounded, Show, Read, Generic, NFData)
  deriving (TextShow) via FromGeneric UnaryFunction

pattern Negate', Abs', Signum', Exp', Log', Sqrt', Sin', Cos', Tan', Asin', Acos', Atan', Sinh', Cosh', Tanh', Asinh', Acosh', Atanh' :: Expression -> Expression
pattern Negate' x = UnaryApply Negate x
pattern Abs' x = UnaryApply Abs x
pattern Signum' x = UnaryApply Signum x
pattern Exp' x = UnaryApply Exp x
pattern Log' x = UnaryApply Log x
pattern Sqrt' x = UnaryApply Sqrt x
pattern Sin' x = UnaryApply Sin x
pattern Cos' x = UnaryApply Cos x
pattern Tan' x = UnaryApply Tan x
pattern Asin' x = UnaryApply Asin x
pattern Acos' x = UnaryApply Acos x
pattern Atan' x = UnaryApply Atan x
pattern Sinh' x = UnaryApply Sinh x
pattern Cosh' x = UnaryApply Cosh x
pattern Tanh' x = UnaryApply Tanh x
pattern Asinh' x = UnaryApply Asinh x
pattern Acosh' x = UnaryApply Acosh x
pattern Atanh' x = UnaryApply Atanh x

data BinaryFunction
  = Add | Multiply | Subtract | Divide | Power | LogBase
  deriving (Eq, Enum, Bounded, Show, Read, Generic, NFData)
  deriving (TextShow) via FromGeneric BinaryFunction

pattern (:+:), (:*:), (:-:), (:/:), (:**:), LogBase' :: Expression -> Expression -> Expression
pattern x :+: y = BinaryApply Add x y
pattern x :*: y = BinaryApply Multiply x y
pattern x :-: y = BinaryApply Subtract x y
pattern x :/: y = BinaryApply Divide x y
pattern x :**: y = BinaryApply Power x y
pattern LogBase' x y = BinaryApply LogBase x y

instance IsString Expression where
  fromString = Symbol . fromString

instance Num Expression where
  (+) = BinaryApply Add
  (-) = BinaryApply Subtract
  (*) = BinaryApply Multiply
  negate = UnaryApply Negate
  abs = UnaryApply Abs
  signum = UnaryApply Signum
  fromInteger = Number

instance Fractional Expression where
  (/) = BinaryApply Divide
  fromRational q | d == 1 = n | otherwise = BinaryApply Divide n d
    where
      n = Number $ numerator q
      d = Number $ denominator q

instance Floating Expression where
  pi = Symbol "pi"
  exp = UnaryApply Exp
  log = UnaryApply Log
  sqrt = UnaryApply Sqrt
  (**) = BinaryApply Power
  logBase = BinaryApply LogBase
  sin = UnaryApply Sin
  cos = UnaryApply Cos
  tan = UnaryApply Tan
  asin = UnaryApply Asin
  acos = UnaryApply Acos
  atan = UnaryApply Atan
  sinh = UnaryApply Sinh
  cosh = UnaryApply Cosh
  tanh = UnaryApply Tanh
  asinh = UnaryApply Asinh
  acosh = UnaryApply Acosh
  atanh = UnaryApply Atanh

getUnaryFunction :: (Floating a) => UnaryFunction -> (a -> a)
getUnaryFunction Negate = negate
getUnaryFunction Abs = abs
getUnaryFunction Signum = signum
getUnaryFunction Exp = exp
getUnaryFunction Log = log
getUnaryFunction Sqrt = sqrt
getUnaryFunction Sin = sin
getUnaryFunction Cos = cos
getUnaryFunction Tan