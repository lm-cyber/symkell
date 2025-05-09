{-# LANGUAGE ForeignFunctionInterface #-}

module Example where

import Data.Char (isUpper)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (free) -- Important for C code to free returned CStrings

fibonacci :: Int -> Int
fibonacci n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

factorial :: Int -> Int
factorial n = product [1..n]

checkString :: String -> String
checkString s = filter isUpper s

fibonacciHS :: CInt -> CInt
fibonacciHS = fromIntegral . fibonacci . fromIntegral

factorialHS :: CInt -> CInt
factorialHS = fromIntegral . factorial . fromIntegral

checkStringHS :: CString -> IO CString
checkStringHS cstr_in = do
    haskell_str <- peekCString cstr_in
    let processed_haskell_str = checkString haskell_str
    newCString processed_haskell_str


foreign export ccall fibonacciHS :: CInt -> CInt
foreign export ccall factorialHS :: CInt -> CInt
foreign export ccall checkStringHS :: CString -> IO CString