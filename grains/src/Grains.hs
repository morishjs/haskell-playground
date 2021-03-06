module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n 
  | n > 64 = Nothing
  | n < 1 = Nothing
  | otherwise = (^) <$> Just 2 <*> Just (n - 1)

total :: Integer
total = fromJust (square 64) * 2 - 1
