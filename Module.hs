module Module where

import Data.Map.Strict ((!), Map)
import Data.Char
import Data.List

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n > 0 = helper (0, 1) n
            | n < 0 = ((-1) ^ ((abs n) + 1)) * (helper (0, 1) (abs n))


helper :: (Integer, Integer) -> Integer -> Integer

helper (_, r) 1 = r
helper (l, r) n = helper (r, l + r) (n - 1)

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = let
    helper (_, _, t) 2 = t
    helper (f, s, t) c = helper (s, t, s + t - (2 * f)) (c - 1)
  in helper (1, 2, 3) n

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count n = helper (0, 0) (abs n)
  where
    helper (s, c) n | n < 10 = (s + n, c + 1)
                    | otherwise = helper (s + mod n 10, c + 1) (div n 10)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper (0.0) 9999
  where
    h = (b - a) / 10000
    helper acc 0 = h * ((((f a) + (f b)) / 2) + acc)
    helper acc n = helper (acc + f (a + (h * n))) (n - 1)

morseCodes :: Map String String
morseCodes = undefined

splitWith :: Eq a => [a] -> [a] -> [[a]]
splitWith x y = func x y [[]]
  where
    func x [] z = reverse $ map (reverse) z
    func x (y:ys) (z:zs) = if (take (length x) (y:ys)) == x then
      func x (drop (length x) (y:ys)) ([]:(z:zs))
    else
      func x ys ((y:z):zs)

decodeMorse :: String -> String
decodeMorse msg = updated
  where
    trimmed = (dropWhileEnd isSpace . dropWhile isSpace) msg
    wordes' = splitWith "   " trimmed
    uncode word = mconcat (map (morseCodes!) (words word))
    updated = unwords (map uncode wordes')