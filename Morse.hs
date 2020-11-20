module Morse where

import Data.Map ((!), Map)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

morseCodes :: Map String String
morseCodes = undefined

splitWith :: Eq a => [a] -> [a] -> [[a]]
splitWith x y = func x y [[]]
  where
    func _ [] z = reverse $ map reverse z
    func x (y:ys) (z:zs) = 
      if take (length x) (y:ys) == x then
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