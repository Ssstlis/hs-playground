module Eratosphen where

import Data.Set as Set (fromList)

--Primes up to n
primes :: Int -> [Int]
primes 2 = [2]
primes 3 = [2, 3]
primes 4 = primes 3
primes 5 = primes 3 ++ [5]
primes 6 = primes 5
primes 7 = primes 5 ++ [7]
primes 8 = primes 7
primes 9 = primes 7
primes n = if n < 2 then [] else primes 9 ++ filter con (takeWhile (<= n) stream)
  where
    set = fromList [2, 3, 5, 7, 9]
    con x = not $ any (\v -> mod x v == 0) set
    up2 n = let x = (n + 2) in x : up2 x
    stream = up2 9
