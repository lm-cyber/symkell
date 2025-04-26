module Symkell.Polynomial.Indexed
  ( IndexedPolynomial,
    IndexedSymbolicPolynomial,
    IndexedPolynomialWith,
  )
where

import Control.DeepSeq (NFData)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Symkell.Polynomial
import Symkell.Symbolic
import TextShow

type IndexedPolynomial = IndexedPolynomialWith Rational

type IndexedSymbolicPolynomial = IndexedPolynomialWith Expression

type IndexedPolynomialWith a = P Int a

newtype P a b = P (IntMap b) deriving (Eq, Generic, NFData)

instance Show (P Int Rational) where
  show = unpack . showt

instance TextShow (P Int Rational) where
  showb (P m)
    | IntMap.null m = "0"
    | otherwise =
        mconcat $
          intersperse " + " $
            map showTerm $
              IntMap.toDescList m
    where
      showTerm (0, 1) = "1"
      showTerm (0, c) = showCoefficient c
      showTerm (1, c) = showCoefficient c <> "x"
      showTerm (e, 1) = "x^" <> showb e
      showTerm (e, c) = showCoefficient c <> "x^" <> showb e
      showCoefficient r
        | 1 <- r = mempty
        | 1 <- denominator r, r > 0 = showb $ numerator r
        | 1 <- denominator r, r < 0 = showbParen True $ showb $ numerator r
        | otherwise = showbParen True $ showb r

instance (Polynomial p e c, TextShow (p e c)) => Show (IndexedPolynomialWith (p e c)) where
  show = unpack . showt

instance (Polynomial p e c, TextShow (p e c)) => TextShow (IndexedPolynomialWith (p e c)) where
  showb (P m)
    | IntMap.null m = "0"
    | otherwise = showb $ IntMap.toList m

instance Show (P Int Expression) where
  show = unpack . showt

instance TextShow (P Int Expression) where
  showb (P m)
    | IntMap.null m = "0"
    | otherwise = showb $ IntMap.toList m

instance (Eq a, Num a) => Num (P Int a) where
  (P p) + (P q) = P $ filterNonzero $ IntMap.unionWith (+) p q
  (P p) * (P q) = P $ filterNonzero $ IntMap.foldlWithKey' accumulate IntMap.empty p
    where
      accumulate m e c = IntMap.unionWith (+) m $ multiplyTerm e c
      multiplyTerm e c = IntMap.mapKeysMonotonic (+ e) $ IntMap.map (* c) q
  abs = id
  signum 0 = 0
  signum _ = 1
  fromInteger 0 = P IntMap.empty
  fromInteger n = P $ IntMap.singleton 0 $ fromInteger n
  negate (P m) = P $ IntMap.map negate m

filterNonzero :: (Eq a, Num a) => IntMap a -> IntMap a
filterNonzero = IntMap.filter (/= 0)

instance (Eq a, Num a) => Polynomial P Int a where
  degree (P m) = maybe 0 fst $ IntMap.lookupMax m
  coefficient (P m) k = fromMaybe 0 $ IntMap.lookup k m
  leadingCoefficient (P m) = maybe 0 snd $ IntMap.lookupMax m
  deleteLeadingTerm (P m) = P $ IntMap.deleteMax m
  foldTerms f (P m) = IntMap.foldMapWithKey f m
  scale 0 _ = P IntMap.empty
  scale x (P m) = P $ IntMap.map (* x) m
  power n = P $ IntMap.singleton (fromIntegral n) 1
