module Stray where

stray :: [Int] -> Int
stray (f : s : tail) 
  | f == s = head (filter (/= f) tail)
  | head tail == f = s 
  | otherwise = f

--  stray = foldr1 xor