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

-- A enhanced expression data type
data Expr = Num Double
          | Var Text
          | Add Expr Expr
          | Mul Expr Expr
          | Pow Expr Expr
          | Neg Expr
          | Sin Expr
          | Cos Expr
          | Tan Expr
          | Log Expr
          | Exp Expr
          | Div Expr Expr
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
differentiate x (Tan e) = Mul (Pow (Cos e) (Num (-2))) (differentiate x e)
differentiate x (Log e) = Mul (Pow e (Num (-1))) (differentiate x e)
differentiate x (Exp e) = Mul (Exp e) (differentiate x e)
differentiate x (Div e1 e2) = Div (Add (Mul (differentiate x e1) e2) (Neg (Mul e1 (differentiate x e2)))) (Pow e2 (Num 2))

-- Basic integration (simplified, would need more complex implementation for real use)
integrate :: Text -> Expr -> Maybe Expr
integrate x (Num c) = Just $ Mul (Num c) (Var x)
integrate x (Var v) | v == x = Just $ Div (Pow (Var x) (Num 2)) (Num 2)
                    | otherwise = Just $ Mul (Var v) (Var x)
integrate x (Add e1 e2) = do
  i1 <- integrate x e1
  i2 <- integrate x e2
  return $ Add i1 i2
integrate x (Mul (Num c) e) = do
  ie <- integrate x e
  return $ Mul (Num c) ie
integrate x (Pow (Var v) (Num n)) | v == x && n /= -1 = 
  Just $ Div (Pow (Var x) (Num (n+1))) (Num (n+1))
integrate x (Sin (Var v)) | v == x = Just $ Neg (Cos (Var x))
integrate x (Cos (Var v)) | v == x = Just $ Sin (Var x)
integrate _ _ = Nothing  -- More complex integrals not implemented

-- Basic simplification
simplify :: Expr -> Expr
simplify (Add (Num 0) e) = simplify e
simplify (Add e (Num 0)) = simplify e
simplify (Mul (Num 0) _) = Num 0
simplify (Mul _ (Num 0)) = Num 0
simplify (Mul (Num 1) e) = simplify e
simplify (Mul e (Num 1)) = simplify e
simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
simplify (Mul e1 e2) = Mul (simplify e1) (simplify e2)
simplify (Pow e (Num 0)) = Num 1
simplify (Pow e (Num 1)) = simplify e
simplify (Neg (Neg e)) = simplify e
simplify (Neg (Num n)) = Num (-n)
simplify (Div e (Num 1)) = simplify e
simplify (Div (Num 0) _) = Num 0
simplify (Div e1 e2) = Div (simplify e1) (simplify e2)
simplify e = e  -- No simplification rule applies

-- Compute limit (very simplified)
limit :: Text -> Expr -> Expr -> Maybe Expr
limit x target expr = 
  case evaluate [(x, Just (\_ -> evaluateExpr target))] expr of
    Just val -> Just (Num val)
    Nothing -> Nothing

-- Taylor series expansion
taylor_series :: Expr -> Text -> Integer -> Int -> Expr
taylor_series expr var point order = 
  let coeffs = [Div (differentiate_n var n expr) (factorial n) | n <- [0..fromIntegral order]]
      terms = [Mul coef (Pow (Add (Var var) (Neg (Num (fromIntegral point)))) (Num (fromIntegral n))) 
              | (coef, n) <- zip coeffs [0..]]
  in foldl Add (Num 0) terms
  where
    differentiate_n var 0 e = substitute var (Num (fromIntegral point)) e
    differentiate_n var n e = differentiate_n var (n-1) (differentiate var e)
    
    factorial 0 = Num 1
    factorial n = Num (fromIntegral n) `Mul` factorial (n-1)

-- Laurent series (simplified)
laurent_series :: Expr -> Text -> Integer -> Int -> Int -> Expr
laurent_series expr var point pos_terms neg_terms =
  let taylor = taylor_series expr var point pos_terms
      -- For negative terms in Laurent series (simplified approach)
      neg_part = Num 0  -- Placeholder for negative terms
  in Add taylor neg_part

-- Substitute variable with expression
substitute :: Text -> Expr -> Expr -> Expr
substitute var val (Var v) | v == var = val
                          | otherwise = Var v
substitute var val (Add e1 e2) = Add (substitute var val e1) (substitute var val e2)
substitute var val (Mul e1 e2) = Mul (substitute var val e1) (substitute var val e2)
substitute var val (Pow e1 e2) = Pow (substitute var val e1) (substitute var val e2)
substitute var val (Neg e) = Neg (substitute var val e)
substitute var val (Sin e) = Sin (substitute var val e)
substitute var val (Cos e) = Cos (substitute var val e)
substitute var val (Tan e) = Tan (substitute var val e)
substitute var val (Log e) = Log (substitute var val e)
substitute var val (Exp e) = Exp (substitute var val e)
substitute var val (Div e1 e2) = Div (substitute var val e1) (substitute var val e2)
substitute _ _ e = e

-- Evaluation (with environment)
type Environment = [(Text, Maybe (Double -> Double))]

evaluate :: Environment -> Expr -> Maybe Double
evaluate _ (Num n) = Just n
evaluate env (Var v) = case lookup v env of
                         Just (Just f) -> Just (f 0)
                         Just Nothing -> Nothing
                         Nothing -> Nothing
evaluate env (Add e1 e2) = (+) <$> evaluate env e1 <*> evaluate env e2
evaluate env (Mul e1 e2) = (*) <$> evaluate env e1 <*> evaluate env e2
evaluate env (Pow e1 e2) = (**) <$> evaluate env e1 <*> evaluate env e2
evaluate env (Neg e) = negate <$> evaluate env e
evaluate env (Sin e) = sin <$> evaluate env e
evaluate env (Cos e) = cos <$> evaluate env e
evaluate env (Tan e) = tan <$> evaluate env e
evaluate env (Log e) = log <$> evaluate env e
evaluate env (Exp e) = exp <$> evaluate env e
evaluate env (Div e1 e2) = (/) <$> evaluate env e1 <*> evaluate env e2

-- Directly evaluate an expression (for numeric values)
evaluateExpr :: Expr -> Double
evaluateExpr expr = case evaluate [] expr of
                      Just val -> val
                      Nothing -> error "Cannot evaluate expression"

-- Basic parsing from string (simplified)
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
    s | T.isInfixOf "/" s ->
        let (left, right) = T.breakOn "/" s
        in Div (parseExpr left) (parseExpr (T.drop 1 right))
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
exprToString (Tan e) = "tan(" <> exprToString e <> ")"
exprToString (Log e) = "log(" <> exprToString e <> ")"
exprToString (Exp e) = "exp(" <> exprToString e <> ")"
exprToString (Div e1 e2) = "(" <> exprToString e1 <> " / " <> exprToString e2 <> ")"

-- Convert C string to Text
cstringToText :: CString -> IO Text
cstringToText cstr = do
  str <- peekCString cstr
  return (T.pack str)

-- Convert Text to C string (caller must free)
textToCString :: Text -> IO CString
textToCString = newCString . T.unpack

-- Safe wrapper for handling errors
safeFunction :: (a -> IO CString) -> a -> IO CString
safeFunction f x = catch (f x) (\(_ :: SomeException) -> return nullPtr)

-- FFI export for differentiation
foreign export ccall differentiate_c :: CString -> CString -> IO CString
differentiate_c :: CString -> CString -> IO CString
differentiate_c varCStr exprCStr = do
  var <- cstringToText varCStr
  expr <- cstringToText exprCStr
  let result = differentiate var (parseExpr expr)
  textToCString (exprToString result)

-- FFI export for integration
foreign export ccall integrate_c :: CString -> CString -> IO CString
integrate_c :: CString -> CString -> IO CString
integrate_c varCStr exprCStr = safeFunction go (varCStr, exprCStr)
  where
    go (vCs, eCs) = do
      var <- cstringToText vCs
      expr <- cstringToText eCs
      case integrate var (parseExpr expr) of
        Just result -> textToCString (exprToString result)
        Nothing -> return nullPtr

-- FFI export for simplification
foreign export ccall simplify_c :: CString -> IO CString
simplify_c :: CString -> IO CString
simplify_c exprCStr = do
  expr <- cstringToText exprCStr
  let result = simplify (parseExpr expr)
  textToCString (exprToString result)

-- FFI export for limit
foreign export ccall limit_c :: CString -> CString -> CString -> IO CString
limit_c :: CString -> CString -> CString -> IO CString
limit_c varCStr targetCStr exprCStr = safeFunction go (varCStr, targetCStr, exprCStr)
  where
    go (vCs, tCs, eCs) = do
      var <- cstringToText vCs
      target <- cstringToText tCs
      expr <- cstringToText eCs
      case limit var (parseExpr target) (parseExpr expr) of
        Just result -> textToCString (exprToString result)
        Nothing -> return nullPtr

-- FFI export for Taylor series
foreign export ccall taylor_series_c :: CString -> CString -> CInt -> CInt -> IO CString
taylor_series_c :: CString -> CString -> CInt -> CInt -> IO CString
taylor_series_c exprCStr varCStr pointC orderC = do
  expr <- cstringToText exprCStr
  var <- cstringToText varCStr
  let point = fromIntegral pointC :: Integer
  let order = fromIntegral orderC :: Int
  let result = taylor_series (parseExpr expr) var point order
  textToCString (exprToString result)

-- FFI export for Laurent series
foreign export ccall laurent_series_c :: CString -> CString -> CInt -> CInt -> CInt -> IO CString
laurent_series_c :: CString -> CString -> CInt -> CInt -> CInt -> IO CString
laurent_series_c exprCStr varCStr pointC posC negC = do
  expr <- cstringToText exprCStr
  var <- cstringToText varCStr
  let point = fromIntegral pointC :: Integer
  let posTerms = fromIntegral posC :: Int
  let negTerms = fromIntegral negC :: Int
  let result = laurent_series (parseExpr expr) var point posTerms negTerms
  textToCString (exprToString result)

-- FFI export for evaluation
foreign export ccall evaluate_c :: CString -> CString -> CDouble -> IO CDouble
evaluate_c :: CString -> CString -> CDouble -> IO CDouble
evaluate_c varCStr exprCStr val = do
  var <- cstringToText varCStr
  exprStr <- cstringToText exprCStr
  let expr = parseExpr exprStr
      env = [(var, Just (\_ -> realToFrac val))]
  case evaluate env expr of
    Just result -> return (realToFrac result)
    Nothing -> return (0/0)  -- NaN for error

-- Initialize and cleanup
foreign export ccall hs_init_c :: IO ()
hs_init_c :: IO ()
hs_init_c = return ()

foreign export ccall hs_exit_c :: IO ()
hs_exit_c :: IO ()
hs_exit_c = return ()

main :: IO ()
main = do
  putStrLn "Symkell FFI Extended Library"
  putStrLn "This is a shared library and not meant to be executed directly." 