module Symkell.Numeric (root) where

root ::
  Integer ->
  Integer ->
  Maybe Integer
root 0 _ = Just 0
root 1 _ = Just 1
root n k
  | k < 0 = Nothing
  | GT <- compare n 0 = search n 1 n
  | LT <- compare n 0, odd k = (* (-1)) <$> search (-n) 1 (-n)
  | otherwise = Nothing
  where
    search m low hi
      | low >= hi, c /= EQ = Nothing
      | EQ <- c = Just mid
      | GT <- c = search m low (mid - 1)
      | LT <- c = search m (mid + 1) hi
      where
        mid = (low + hi) `div` 2
        c = compare (mid ^ k) m
