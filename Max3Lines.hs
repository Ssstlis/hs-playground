module Max3Lines where

import Data.List (transpose)

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 a b c = map maximum (transpose [a, b, c])