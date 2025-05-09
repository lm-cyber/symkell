module Symkell.Symbolic.LaTeX (toLaTeX) where

import Data.Text (Text)
import Symkell.Symbolic
import TextShow (showt)

toLaTeX :: Expression -> Text
toLaTeX (Number n) = showt n
toLaTeX (Symbol "pi") = "\\pi"
toLaTeX (Symbol s) = s
toLaTeX (UnaryApply func x) = unary func x
toLaTeX (BinaryApply func x y) = binary func x y

unary :: UnaryFunction -> Expression -> Text
unary Negate x@(_ :+: _) = "-" <> asArg x
unary Negate x@(_ :-: _) = "-" <> asArg x
unary Negate x@(Negate' _) = "-" <> asArg x
unary Negate x = "-" <> toLaTeX x
unary Abs x = "\\left\\lvert " <> toLaTeX x <> " \\right\\rvert"
unary Signum x = "\\mathrm{signum}" <> par (toLaTeX x)
unary Exp x = "e^" <> brace (toLaTeX x)
unary Log x = "\\log " <> asNamedFunctionArg x
unary Sqrt x = "\\sqrt" <> brace (toLaTeX x)
unary Sin x = "\\sin " <> asNamedFunctionArg x
unary Cos x = "\\cos " <> asNamedFunctionArg x
unary Tan x = "\\tan " <> asNamedFunctionArg x
unary Asin x = "\\sin^{-1} " <> asNamedFunctionArg x
unary Acos x = "\\cos^{-1} " <> asNamedFunctionArg x
unary Atan x = "\\tan^{-1} " <> asNamedFunctionArg x
unary Sinh x = "\\sinh " <> asNamedFunctionArg x
unary Cosh x = "\\cosh " <> asNamedFunctionArg x
unary Tanh x = "\\tanh " <> asNamedFunctionArg x
unary Asinh x = "\\sinh^{-1} " <> asNamedFunctionArg x
unary Acosh x = "\\cosh^{-1} " <> asNamedFunctionArg x
unary Atanh x = "\\tanh^{-1} " <> asNamedFunctionArg x

binary :: BinaryFunction -> Expression -> Expression -> Text
binary Add x (Negate' y) = binary Subtract x y
binary Add x y = asAddInitialArg x <> " + " <> asAddTrailingArg y
binary Multiply x@(_ :*: Number _) y@(Number _ :*: _) = toLaTeX x <> " \\times " <> toLaTeX y
binary Multiply x@(Number _) y@(Number _ :*: _) = toLaTeX x <> " \\times " <> toLaTeX y
binary Multiply x@(_ :*: Number _) y@(Number _) = toLaTeX x <> " \\times " <> toLaTeX y
binary Multiply x y@(Number _) = asMultiplyArg x <> " \\times " <> asArg y
binary Multiply x@(Abs' _) y = toLaTeX x <> " " <> asMultiplyArg y
binary Multiply x@(Signum' _) y = toLaTeX x <> " " <> asMultiplyArg y
binary Multiply x@(Exp' _) y = toLaTeX x <> " " <> asMultiplyArg y
binary Multiply x@(Sqrt' _) y = toLaTeX x <> " " <> asMultiplyArg y
binary Multiply x@(UnaryApply _ _) y@(Symbol _) = par (toLaTeX x) <> " " <> asMultiplyArg y
binary Multiply x@(LogBase' _ _) y = par (toLaTeX x) <> " " <> asMultiplyArg y
binary Multiply x y = asMultiplyArg x <> " " <> asMultiplyArg y
binary Subtract x y@(Negate' _) = asAddInitialArg x <> " - " <> asArg y
binary Subtract x y@(_ :+: _) = asAddInitialArg x <> " - " <> asArg y
binary Subtract x y@(_ :-: _) = asAddInitialArg x <> " - " <> asArg y
binary Subtract x y = asAddInitialArg x <> " - " <> asAddTrailingArg y
binary Divide x y = "\\frac" <> brace (toLaTeX x) <> brace (toLaTeX y)
binary Power x y = asArg x <> "^" <> brace (toLaTeX y)
binary LogBase x y = "\\log_" <> brace (toLaTeX x) <> asNamedFunctionArg y

asArg :: Expression -> Text
asArg e@(Number n) | n >= 0 = toLaTeX e | otherwise = par $ toLaTeX e
asArg e@(Symbol _) = toLaTeX e
asArg e@(Negate' _) = par $ toLaTeX e
asArg e@(UnaryApply _ _) = toLaTeX e
asArg e@(_ :/: _) = toLaTeX e
asArg e@(Number _ :**: _) = par $ toLaTeX e
asArg e@(_ :**: _) = toLaTeX e
asArg e = par $ toLaTeX e

asAddInitialArg :: Expression -> Text
asAddInitialArg e@(Number _) = toLaTeX e
asAddInitialArg e@(Symbol _) = toLaTeX e
asAddInitialArg e@(Negate' _) = toLaTeX e
asAddInitialArg (x :+: y) = asAddInitialArg x <> " + " <> asAddTrailingArg y
asAddInitialArg (x :-: y@(Negate' _)) = asAddInitialArg x <> " - " <> asArg y
asAddInitialArg (x :-: y@(_ :+: _)) = asAddInitialArg x <> " - " <> asArg y
asAddInitialArg (x :-: y@(_ :-: _)) = asAddInitialArg x <> " - " <> asArg y
asAddInitialArg (x :-: y) = asAddInitialArg x <> " - " <> toLaTeX y
asAddInitialArg e = asAddTrailingArg e

asAddTrailingArg :: Expression -> Text
asAddTrailingArg e@(Number _) = asArg e
asAddTrailingArg e@(Symbol _) = asArg e
asAddTrailingArg e@(Negate' _) = asArg e
asAddTrailingArg e = toLaTeX e

asMultiplyArg :: Expression -> Text
asMultiplyArg e@(Number _) = asArg e
asMultiplyArg e@(Symbol _) = asArg e
asMultiplyArg e@(Negate' _) = asArg e
asMultiplyArg e@(UnaryApply _ _) = toLaTeX e
asMultiplyArg e@(_ :+: _) = asArg e
asMultiplyArg e@(_ :-: _) = asArg e
asMultiplyArg e@(BinaryApply _ _ _) = toLaTeX e

asNamedFunctionArg :: Expression -> Text
asNamedFunctionArg e@(Exp' _) = asArg e
asNamedFunctionArg e@(Abs' _) = asArg e
asNamedFunctionArg e@(Sqrt' _) = asArg e
asNamedFunctionArg e@(UnaryApply _ _) = par $ toLaTeX e
asNamedFunctionArg e@(LogBase' _ _) = par $ toLaTeX e
asNamedFunctionArg e = asArg e

par :: Text -> Text
par s = "\\left(" <> s <> "\\right)"

brace :: Text -> Text
brace s = "{" <> s <> "}"
