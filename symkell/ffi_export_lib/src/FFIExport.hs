{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module FFIExport where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Data.Text.Encoding as TE

-- Import the main SymkellCore module
import SymkellCore
  ( Expression(..)
  , differentiate
  , integrate
  , evaluate
  , simplify
  , limit
  , taylorSeries
  , laurentSeries
  )

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

-- Wrapper for differentiation
foreign export ccall symkell_differentiate :: CString -> CString -> IO CString
symkell_differentiate :: CString -> CString -> IO CString
symkell_differentiate varCs exprCs = do
  var <- cStringToText varCs
  exprMaybe <- parseExpression exprCs
  case exprMaybe of
    Just expr -> expressionToCString $ differentiate var expr
    Nothing -> newCString "Error: Failed to parse expression"

-- Wrapper for integration
foreign export ccall symkell_integrate :: CString -> CString -> IO CString
symkell_integrate :: CString -> CString -> IO CString
symkell_integrate varCs exprCs = do
  var <- cStringToText varCs
  exprMaybe <- parseExpression exprCs
  case exprMaybe of
    Just expr -> case integrate var expr of
      Just result -> expressionToCString result
      Nothing -> newCString "Error: Failed to integrate expression"
    Nothing -> newCString "Error: Failed to parse expression"

-- Wrapper for simplification
foreign export ccall symkell_simplify :: CString -> IO CString
symkell_simplify :: CString -> IO CString
symkell_simplify exprCs = do
  exprMaybe <- parseExpression exprCs
  case exprMaybe of
    Just expr -> expressionToCString $ simplify expr
    Nothing -> newCString "Error: Failed to parse expression"

-- Wrapper for limit
foreign export ccall symkell_limit :: CString -> CString -> CString -> IO CString
symkell_limit :: CString -> CString -> CString -> IO CString
symkell_limit varCs targetCs exprCs = do
  var <- cStringToText varCs
  exprMaybe <- parseExpression exprCs
  targetMaybe <- parseExpression targetCs
  case (exprMaybe, targetMaybe) of
    (Just expr, Just target) -> case limit var target expr of
      Just result -> expressionToCString result
      Nothing -> newCString "Error: Failed to compute limit"
    _ -> newCString "Error: Failed to parse expressions"

-- Wrapper for Taylor series
foreign export ccall symkell_taylor_series :: CString -> CString -> CInt -> CInt -> IO CString
symkell_taylor_series :: CString -> CString -> CInt -> CInt -> IO CString
symkell_taylor_series exprCs varCs pointC orderC = do
  var <- cStringToText varCs
  exprMaybe <- parseExpression exprCs
  let point = fromIntegral pointC :: Integer
  let order = fromIntegral orderC :: Int
  case exprMaybe of
    Just expr -> expressionToCString $ taylorSeries expr var point order
    Nothing -> newCString "Error: Failed to parse expression"

-- Wrapper for Laurent series
foreign export ccall symkell_laurent_series :: CString -> CString -> CInt -> CInt -> CInt -> IO CString
symkell_laurent_series :: CString -> CString -> CInt -> CInt -> CInt -> IO CString
symkell_laurent_series exprCs varCs pointC posC negC = do
  var <- cStringToText varCs
  exprMaybe <- parseExpression exprCs
  let point = fromIntegral pointC :: Integer
  let posTerms = fromIntegral posC :: Int
  let negTerms = fromIntegral negC :: Int
  case exprMaybe of
    Just expr -> expressionToCString $ laurentSeries expr var point posTerms negTerms
    Nothing -> newCString "Error: Failed to parse expression"

-- Wrapper for evaluation
-- Note: We're using simple variable-value pairs for evaluation
-- foreign export ccall symkell_evaluate :: CString -> CString -> CString -> IO CDouble
-- symkell_evaluate :: CString -> CString -> CString -> IO CDouble
-- symkell_evaluate exprCs varCs valCs = do
--   var <- cStringToText varCs
--   exprMaybe <- parseExpression exprCs
--   valMaybe <- parseExpression valCs
--   case (exprMaybe, valMaybe) of
--     (Just expr, Just (Number val)) -> 
--       case evaluate expr (\v -> if v == var then Just (fromIntegral val :: Double) else Nothing) of
--         Just result -> return (realToFrac result)
--         Nothing -> return (0/0) -- NaN to indicate error
--     _ -> return (0/0) -- NaN to indicate error

-- Initialize and clean up the Haskell RTS
foreign export ccall hs_init_symkell :: IO ()
hs_init_symkell :: IO ()
hs_init_symkell = return ()  -- RTS will be initialized by hs_init in Python

foreign export ccall hs_exit_symkell :: IO ()
hs_exit_symkell :: IO ()
hs_exit_symkell = return ()  -- Optional cleanup 