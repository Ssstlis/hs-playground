module ReSorting where

import Data.List (nub,sort, partition)

menFromBoys :: [Int] -> [Int]
menFromBoys xs = case (partition even . nub) xs of
    (evn, od) -> sort evn ++ reverse (sort od)