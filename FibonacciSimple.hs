module FibonacciSimple where

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
                    | n > 0 = helper (0, 1) n
                    | n < 0 = ((-1) ^ ((abs n) + 1)) * helper (0, 1) (abs n)


helper :: (Integer, Integer) -> Integer -> Integer

helper (_, r) 1 = r
helper (l, r) n = helper (r, l + r) (n - 1)