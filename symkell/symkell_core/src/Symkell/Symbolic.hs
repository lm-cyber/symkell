{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
module Symkell.Symbolic
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
import Data.Text
import GHC.Generics (Generic)
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))
data Expression
  = 
    Number Integer
  | 
    Symbol Text
  | 
    UnaryApply UnaryFunction Expression
  | 
    BinaryApply BinaryFunction Expression Expression
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
  = 
    Negate
  | 
    Abs
  | 
    Signum
  | 
    Exp
  | 
    Log
  | 
    Sqrt
  | 
    Sin
  | 
    Cos
  | 
    Tan
  | 
    Asin
  | 
    Acos
  | 
    Atan
  | 
    Sinh
  | 
    Cosh
  | 
    Tanh
  | 
    Asinh
  | 
    Acosh
  | 
    Atanh
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
  = 
    Add
  | 
    Multiply
  | 
    Subtract
  | 
    Divide
  | 
    Power
  | 
    LogBase
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
getUnaryFunction Tan = tan
getUnaryFunction Asin = asin
getUnaryFunction Acos = acos
getUnaryFunction Atan = atan
getUnaryFunction Sinh = sinh
getUnaryFunction Cosh = cosh
getUnaryFunction Tanh = tanh
getUnaryFunction Asinh = asinh
getUnaryFunction Acosh = acosh
getUnaryFunction Atanh = atanh
getBinaryFunction :: (Floating a) => BinaryFunction -> (a -> a -> a)
getBinaryFunction Add = (+)
getBinaryFunction Multiply = (*)
getBinaryFunction Subtract = (-)
getBinaryFunction Divide = (/)
getBinaryFunction Power = (**)
getBinaryFunction LogBase = logBase
substitute ::
  Expression ->
  (Text -> Maybe Expression) ->
  Expression
substitute e@(Number _) _ = e
substitute e@(Symbol s) f
  | (Just x) <- f s = x
  | otherwise = e
substitute (UnaryApply func x) f = UnaryApply func (substitute x f)
substitute (BinaryApply func x y) f = BinaryApply func (substitute x f) (substitute y f)
evaluate ::
  (Floating a) =>
  Expression ->
  (Text -> Maybe a) ->
  Maybe a
evaluate (Number n) _ = Just $ fromInteger n
evaluate (Symbol "pi") _ = Just pi
evaluate (Symbol x) m = m x
evaluate (UnaryApply fun expr) m = fmap f v
  where
    f = getUnaryFunction fun
    v = evaluate expr m
evaluate (BinaryApply fun expr1 expr2) m = f <$> v1 <*> v2
  where
    f = getBinaryFunction fun
    v1 = evaluate expr1 m
    v2 = evaluate expr2 m
fractionalEvaluate ::
  (Eq a, Fractional a) =>
  Expression ->
  (Text -> Maybe a) ->
  Maybe a
fractionalEvaluate (Number n) _ = Just $ fromInteger n
fractionalEvaluate (Symbol x) m = m x
fractionalEvaluate (Negate' x) m = negate <$> fractionalEvaluate x m
fractionalEvaluate (Abs' x) m = abs <$> fractionalEvaluate x m
fractionalEvaluate (Signum' x) m = signum <$> fractionalEvaluate x m
fractionalEvaluate (x :+: y) m = (+) <$> fractionalEvaluate x m <*> fractionalEvaluate y m
fractionalEvaluate (x :-: y) m = (-) <$> fractionalEvaluate x m <*> fractionalEvaluate y m
fractionalEvaluate (x :*: y) m = (*) <$> fractionalEvaluate x m <*> fractionalEvaluate y m
fractionalEvaluate (x :/: y) m
  | Just 0 <- y' = Nothing
  | otherwise = (/) <$> x' <*> y'
  where
    x' = fractionalEvaluate x m
    y' = fractionalEvaluate y m
fractionalEvaluate (x :**: (Number n)) m = (^^ n) <$> fractionalEvaluate x m
fractionalEvaluate _ _ = Nothing
toFunction ::
  (Floating b) =>
  Expression ->
  (Text -> (a -> b)) ->
  (a -> b)
toFunction (Number n) _ = const $ fromInteger n
toFunction (Symbol s) m = m s
toFunction (UnaryApply func x) m = f . g
  where
    f = getUnaryFunction func
    g = toFunction x m
toFunction (BinaryApply func x y) m = \v -> f (g v) (h v)
  where
    f = getBinaryFunction func
    g = toFunction x m
    h = toFunction y m