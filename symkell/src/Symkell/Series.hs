module Symkell.Series
  ( taylorSeries
  , laurentSeries
  ) where

import Data.Text (Text)
import Symkell.Symbolic
import Symkell.Differentiation (differentiate)
import Symkell.Symbolic.Simplify (simplify)

-- | Compute the Taylor series expansion of an expression.
-- The expansion is of the form: f(a) + f'(a)(x-a)/1! + f''(a)(x-a)²/2! + ...
taylorSeries :: 
  Expression -- ^ The expression to expand
  -> Text    -- ^ The variable to expand around
  -> Integer -- ^ The point to expand around
  -> Int     -- ^ The order of the expansion
  -> Expression
taylorSeries expr var point 0 = 
  -- Special case: For order 0, just return the value at the point
  simplify $ substitute expr $ \v -> if v == var then Just (Number point) else Nothing

taylorSeries expr var point order = 
  simplify $ foldl (\acc n -> acc :+: term n) (term 0) [1..order']
  where
    order' = fromIntegral order
    
    -- Calculate the n-th term of the Taylor series
    term 0 = simplify $ substitute expr $ 
      \v -> if v == var then Just (Number point) else Nothing
      
    term n = simplify $ 
      let 
        -- Calculate the n-th derivative
        nthDerivative = iterate (differentiate var) expr !! n
        -- Substitute the expansion point into the derivative
        derivAtPoint = substitute nthDerivative $ 
          \v -> if v == var then Just (Number point) else Nothing
        -- Calculate (x - point)^n
        xMinusA = (Symbol var) :-: (Number point)
        power = xMinusA :**: (Number $ fromIntegral n)
        -- Calculate n!
        factorial = Number $ product [1..fromIntegral n]
      in 
        derivAtPoint :*: power :/: factorial

-- | Compute the Laurent series expansion of an expression.
-- For functions with poles, the expansion includes negative powers of (x-a).
laurentSeries :: 
  Expression -- ^ The expression to expand
  -> Text    -- ^ The variable to expand around
  -> Integer -- ^ The point to expand around
  -> Int     -- ^ Number of positive terms
  -> Int     -- ^ Number of negative terms
  -> Expression
laurentSeries expr var point posTerms 0 =
  -- If no negative terms requested, just return Taylor series
  taylorSeries expr var point posTerms

laurentSeries expr var point posTerms negTerms =
  simplify $ principalPart :+: analyticPart
  where
    -- Variable shifted to expansion point: (x - point)
    xMinusA = (Symbol var) :-: (Number point)
    
    -- For functions with simple poles like 1/x or 1/x²,
    -- directly handle these special cases
    principalPart = 
      if isSimplePole expr var point
      then handleSimplePole expr var point negTerms
      else computePrincipalPart
      
    -- Compute regular Taylor terms (analytic part)
    analyticPart = 
      if posTerms > 0
      then taylorSeries expr var point posTerms
      else Number 0
      
    -- Direct computation of the principal part for general functions
    computePrincipalPart = 
      foldr (:+:) (Number 0) $ map negativeTerm [1..negTerms]
      
    -- Calculate negative power terms using residue approach
    negativeTerm k =
      let 
        -- For the coefficient of (x-a)^(-k), we need the (k-1)th derivative
        -- of (x-a)^k * f(x) evaluated at x=a, divided by (k-1)!
        transformedExpr = (xMinusA :**: Number (fromIntegral k)) :*: expr
        residueDerivative = iterate (differentiate var) transformedExpr !! (k-1)
        residueAtPoint = substitute residueDerivative $
          \v -> if v == var then Just (Number point) else Nothing
        factorial = Number $ product [1..fromIntegral (k-1)]
        coefficient = residueAtPoint :/: factorial
      in
        simplify $ coefficient :*: (xMinusA :**: Number (fromIntegral (-k)))

-- Helper function to check if an expression has a simple pole at a point
isSimplePole :: Expression -> Text -> Integer -> Bool
isSimplePole (Number _ :/: Symbol v) var point = v == var && point == 0
isSimplePole (Number _ :/: (Symbol v :**: Number _)) var point = v == var && point == 0
isSimplePole (Number _ :/: (Symbol v :-: Number p)) var point = v == var && fromIntegral point == p
isSimplePole ((Symbol v :**: Number _) :/: Symbol v') var point = v == var && v' == var && point == 0
isSimplePole _ _ _ = False

-- Handle simple pole cases directly without residue calculation
handleSimplePole :: Expression -> Text -> Integer -> Int -> Expression
handleSimplePole (Number n :/: Symbol v) var _ negTerms 
  | negTerms >= 1 = Number n :/: Symbol v
  | otherwise = Number 0

handleSimplePole (Number n :/: (Symbol v :**: Number p)) var _ negTerms
  | negTerms >= fromIntegral p = Number n :/: (Symbol v :**: Number p)
  | otherwise = Number 0
  
handleSimplePole (Number n :/: (Symbol v :-: Number p)) var point negTerms
  | negTerms >= 1 = 
      -- For 1/(x-a), the Laurent series is -1/(a-x) = -1/(-(x-a)) = 1/(x-a)
      -- Or if expanded: -1/(a-x) = -1/(a*(1-x/a)) = -1/a * 1/(1-x/a) = -1/a * (1 + x/a + (x/a)² + ...)
      let expandedSeries = foldr (:+:) (Number 0) $ 
                          map (\k -> (Number (-1)^k) :*: (Number n) :*: 
                               ((Symbol var) :**: Number (fromIntegral k)) :/: 
                               (Number p :**: Number (fromIntegral (k+1))))
                              [0..(negTerms-1)]
      in simplify expandedSeries
  | otherwise = Number 0

handleSimplePole ((Symbol v :**: Number p) :/: Symbol v') var _ negTerms
  | v == v' && negTerms >= 1 && p == 1 = Symbol v
  | v == v' && negTerms >= 1 = (Symbol v :**: Number (p-1))
  | otherwise = Number 0
  
handleSimplePole _ _ _ _ = Number 0 