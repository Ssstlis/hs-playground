module Tribonacci where

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = let
    helper (_, _, t) 2 = t
    helper (f, s, t) c = helper (s, t, s + t - (2 * f)) (c - 1)
  in helper (1, 2, 3) n