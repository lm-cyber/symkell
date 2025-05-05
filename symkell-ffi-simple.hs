{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.C.String
import Foreign.C.Types
import Control.Exception (catch, SomeException)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- Simple Expression type (without TextShow dependencies)
data Expression 
  = Number Integer
  | Symbol Text
  | UnaryApply UnaryFunction Expression 
  | BinaryApply BinaryFunction Expression Expression
  deriving (Eq, Show, Read)

data UnaryFunction 
  = Negate
  | Sin
  | Cos
  deriving (Eq, Show, Read)

data BinaryFunction 
  = Add
  | Multiply
  | Subtract
  deriving (Eq, Show, Read)

-- Simple differentiation
differentiate :: Text -> Expression -> Expression
differentiate var expr = case expr of
  Number _ -> Number 0  -- Derivative of constant is 0
  Symbol s | s == var -> Number 1  -- Derivative of x is 1
  Symbol _ -> Number 0  -- Derivative of other variables is 0
  UnaryApply Sin e -> BinaryApply Multiply (differentiate var e) (UnaryApply Cos e)
  UnaryApply Cos e -> BinaryApply Multiply 
                          (UnaryApply Negate (differentiate var e))
                          (UnaryApply Sin e)
  UnaryApply Negate e -> UnaryApply Negate (differentiate var e)
  BinaryApply Add e1 e2 -> BinaryApply Add (differentiate var e1) (differentiate var e2)
  BinaryApply Subtract e1 e2 -> BinaryApply Subtract (differentiate var e1) (differentiate var e2)
  BinaryApply Multiply e1 e2 -> 
    BinaryApply Add 
      (BinaryApply Multiply (differentiate var e1) e2)
      (BinaryApply Multiply e1 (differentiate var e2))

-- Simple evaluation
evaluate :: Expression -> (Text -> Maybe Double) -> Maybe Double
evaluate (Number n) _ = Just $ fromInteger n
evaluate (Symbol x) m = m x
evaluate (UnaryApply Negate e) m = negate <$> evaluate e m
evaluate (UnaryApply Sin e) m = sin <$> evaluate e m
evaluate (UnaryApply Cos e) m = cos <$> evaluate e m
evaluate (BinaryApply Add e1 e2) m = (+) <$> evaluate e1 m <*> evaluate e2 m
evaluate (BinaryApply Subtract e1 e2) m = (-) <$> evaluate e1 m <*> evaluate e2 m
evaluate (BinaryApply Multiply e1 e2) m = (*) <$> evaluate e1 m <*> evaluate e2 m

-- Helper for converting Text to CString
textToCString :: Text -> IO CString
textToCString = newCString . T.unpack

-- Helper for converting CString to Text
cStringToText :: CString -> IO Text
cStringToText cs = do
  str <- peekCString cs
  return (T.pack str)

-- Parse an expression from a string
parseExpression :: CString -> IO (Maybe Expression)
parseExpression cs = do
  txt <- cStringToText cs
  case reads (T.unpack txt) of
    [(expr, "")] -> return (Just expr)
    _ -> return Nothing

-- Convert an expression to a string
expressionToCString :: Expression -> IO CString
expressionToCString expr = newCString (show expr)

-- Safe wrapper for handling exceptions
safeFunc :: (a -> IO CString) -> a -> IO CString
safeFunc f x = catch (f x) (\(_ :: SomeException) -> newCString "Error: Exception occurred")

-- Wrapper for differentiation
foreign export ccall symkell_differentiate :: CString -> CString -> IO CString
symkell_differentiate :: CString -> CString -> IO CString
symkell_differentiate varCs exprCs = safeFunc go (varCs, exprCs)
  where
    go (vCs, eCs) = do
      var <- cStringToText vCs
      exprMaybe <- parseExpression eCs
      case exprMaybe of
        Just expr -> expressionToCString $ differentiate var expr
        Nothing -> newCString "Error: Failed to parse expression"

-- Wrapper for evaluation
foreign export ccall symkell_evaluate :: CString -> CString -> CString -> IO CDouble
symkell_evaluate :: CString -> CString -> CString -> IO CDouble
symkell_evaluate exprCs varCs valCs = do
  result <- catch (go exprCs varCs valCs) (\(_ :: SomeException) -> return (0/0))
  return result
  where
    go eCs vCs valCs = do
      var <- cStringToText vCs
      exprMaybe <- parseExpression eCs
      valMaybe <- parseExpression valCs
      case (exprMaybe, valMaybe) of
        (Just expr, Just (Number val)) -> 
          case evaluate expr (\v -> if v == var then Just (fromIntegral val) else Nothing) of
            Just result -> return (realToFrac result)
            Nothing -> return (0/0) -- NaN to indicate error
        _ -> return (0/0) -- NaN to indicate error

-- Initialize and clean up the Haskell RTS
foreign export ccall hs_init_symkell :: IO ()
hs_init_symkell :: IO ()
hs_init_symkell = return ()  -- RTS will be initialized by hs_init in Python

foreign export ccall hs_exit_symkell :: IO ()
hs_exit_symkell :: IO ()
hs_exit_symkell = return ()  -- Optional cleanup

main :: IO ()
main = do
  putStrLn "Symkell FFI Library (Simple Version)"
  putStrLn "This is a shared library and not meant to be executed directly." 