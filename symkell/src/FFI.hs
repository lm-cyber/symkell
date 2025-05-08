{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module FFI where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Data.Text.Encoding as TE

import Symkell.Differentiation (differentiate)
import Symkell.Integration qualified as Integration
import Symkell.Symbolic (Expression, evaluate, fractionalEvaluate, toFunction)
import Symkell.Symbolic.Haskell (toHaskell)
import Symkell.Symbolic.LaTeX (toLaTeX)
import Symkell.Symbolic.Simplify (simplify, simplifyForVariable)
import Symkell.Symbolic.Simplify.Tidy (tidy)
import Symkell.Limit (limit)

fibonacci :: Int -> Int
fibonacci n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fibonacciHS :: CInt -> CInt
fibonacciHS = fromIntegral . fibonacci . fromIntegral
foreign export ccall fibonacciHS :: CInt -> CInt


-- Wrapper for differentiation
-- Wrapper for integration
-- Wrapper for simplification
-- Wrapper for limit    
-- Wrapper for Taylor series
-- Wrapper for Laurent series
-- Wrapper for evaluation
-- Wrapper for haskell
-- Wrapper for latex
-- Wrapper for tidy
