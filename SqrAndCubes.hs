module SqrAndCubes where

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = foldl (\a e -> a ++ [e * e, e * e * e]) []