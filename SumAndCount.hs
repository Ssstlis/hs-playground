module SumAndCount where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n = helper (0, 0) (abs n)
  where
    helper (s, c) r | r < 10 = (s + r, c + 1)
                             | otherwise = helper (s + mod r 10, c + 1) (div r 10)