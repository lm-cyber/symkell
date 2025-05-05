{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Data.Text.IO as TIO

-- A simplified expression data type
data Expr = Num Double
          | Var Text
          | Add Expr Expr
          | Mul Expr Expr
          | Pow Expr Expr
          | Neg Expr
          | Sin Expr
          | Cos Expr
          deriving (Show, Eq)

-- Basic differentiation
differentiate :: Text -> Expr -> Expr
differentiate x (Num _) = Num 0
differentiate x (Var v) | v == x = Num 1
                       | otherwise = Num 0
differentiate x (Add e1 e2) = Add (differentiate x e1) (differentiate x e2)
differentiate x (Mul e1 e2) = Add (Mul (differentiate x e1) e2) (Mul e1 (differentiate x e2))
differentiate x (Pow e (Num n)) = Mul (Mul (Num n) (Pow e (Num (n-1)))) (differentiate x e)
differentiate x (Neg e) = Neg (differentiate x e)
differentiate x (Sin e) = Mul (Cos e) (differentiate x e)
differentiate x (Cos e) = Mul (Neg (Sin e)) (differentiate x e)
differentiate x e = error $ "Cannot differentiate: " ++ show e

-- Basic evaluation with variable substitution
evaluate :: [(Text, Double)] -> Expr -> Double
evaluate env (Num n) = n
evaluate env (Var v) = case lookup v env of
                         Just val -> val
                         Nothing -> error $ "Variable not found: " ++ T.unpack v
evaluate env (Add e1 e2) = evaluate env e1 + evaluate env e2
evaluate env (Mul e1 e2) = evaluate env e1 * evaluate env e2
evaluate env (Pow e1 e2) = evaluate env e1 ** evaluate env e2
evaluate env (Neg e) = -(evaluate env e)
evaluate env (Sin e) = sin (evaluate env e)
evaluate env (Cos e) = cos (evaluate env e)

-- Parse a simple expression from string
parseExpr :: Text -> Expr
parseExpr str = 
  case T.strip str of
    "x" -> Var "x"
    "y" -> Var "y"
    "z" -> Var "z"
    s | T.isPrefixOf "sin(" s && T.isSuffixOf ")" s ->
        let inner = T.drop 4 (T.take (T.length s - 1) s)
        in Sin (parseExpr inner)
    s | T.isPrefixOf "cos(" s && T.isSuffixOf ")" s ->
        let inner = T.drop 4 (T.take (T.length s - 1) s)
        in Cos (parseExpr inner)
    s | T.isPrefixOf "-" s ->
        Neg (parseExpr (T.drop 1 s))
    s | T.isInfixOf "+" s ->
        let (left, right) = T.breakOn "+" s
        in Add (parseExpr left) (parseExpr (T.drop 1 right))
    s | T.isInfixOf "*" s ->
        let (left, right) = T.breakOn "*" s
        in Mul (parseExpr left) (parseExpr (T.drop 1 right))
    s | T.isInfixOf "^" s ->
        let (left, right) = T.breakOn "^" s
        in Pow (parseExpr left) (parseExpr (T.drop 1 right))
    s -> case (readMaybe (T.unpack s) :: Maybe Double) of
           Just n -> Num n
           Nothing -> Var s
  where
    readMaybe s = case reads s of
                    [(x, "")] -> Just x
                    _ -> Nothing

-- Convert expression to string
exprToString :: Expr -> Text
exprToString (Num n) = T.pack (show n)
exprToString (Var v) = v
exprToString (Add e1 e2) = "(" <> exprToString e1 <> " + " <> exprToString e2 <> ")"
exprToString (Mul e1 e2) = "(" <> exprToString e1 <> " * " <> exprToString e2 <> ")"
exprToString (Pow e1 e2) = "(" <> exprToString e1 <> " ^ " <> exprToString e2 <> ")"
exprToString (Neg e) = "(-" <> exprToString e <> ")"
exprToString (Sin e) = "sin(" <> exprToString e <> ")"
exprToString (Cos e) = "cos(" <> exprToString e <> ")"

-- Convert C string to Text
cstringToText :: CString -> IO Text
cstringToText cstr = do
  str <- peekCString cstr
  return (T.pack str)

-- Convert Text to C string (caller must free)
textToCString :: Text -> IO CString
textToCString = newCString . T.unpack

-- FFI export for differentiation
foreign export ccall differentiate_c :: CString -> CString -> IO CString
differentiate_c :: CString -> CString -> IO CString
differentiate_c varCStr exprCStr = do
  bracket (do var <- cstringToText varCStr
              expr <- cstringToText exprCStr
              return (var, expr))
          (\_ -> return ())
          (\(var, exprStr) -> do
              let expr = parseExpr exprStr
                  result = differentiate var expr
              textToCString (exprToString result)
          )

-- FFI export for evaluation
foreign export ccall evaluate_c :: CString -> CString -> CDouble -> IO CDouble
evaluate_c :: CString -> CString -> CDouble -> IO CDouble
evaluate_c varCStr exprCStr val = do
  var <- cstringToText varCStr
  exprStr <- cstringToText exprCStr
  let expr = parseExpr exprStr
      env = [(var, realToFrac val)]
  return (realToFrac (evaluate env expr))

-- Initialize and cleanup
foreign export ccall hs_init_c :: IO ()
hs_init_c :: IO ()
hs_init_c = return ()

foreign export ccall hs_exit_c :: IO ()
hs_exit_c :: IO ()
hs_exit_c = return ()

main :: IO ()
main = do
  putStrLn "Symbolic mathematics FFI library"
  putStrLn "This is a standalone program for testing."
  
  let expr = Add (Pow (Var "x") (Num 2)) (Mul (Num 2) (Var "x"))
      diff = differentiate "x" expr
  
  putStrLn $ "Original: " ++ T.unpack (exprToString expr)
  putStrLn $ "Derivative: " ++ T.unpack (exprToString diff)
  
  let x = 2.0
      env = [("x", x)]
      val = evaluate env expr
      dval = evaluate env diff
  
  putStrLn $ "f(" ++ show x ++ ") = " ++ show val
  putStrLn $ "f'(" ++ show x ++ ") = " ++ show dval 