module Rot13 where

import Data.Char (chr, isLetter, isLower, ord)

rot13 :: String -> String
rot13 = map (\c -> if (isLetter c) then chr $ f c else c)
  where
    lwF = ord 'a'
    upF = ord 'A'
    rg = 26
    re b pos = if (pos < b) then 26 + pos else pos
    f lt = let pos = (ord lt) - 13 in
      (if (isLower lt) then re lwF else re upF) pos