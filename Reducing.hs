module Reducing where

import Data.List

reduceByRules :: [Double] -> [Double -> Double -> Double] -> Double
reduceByRules numbers functions = g numbers
  where
   f acc [] _ = acc
   f acc xs [] = f acc xs functions
   f acc (x : xs) (fn : fns) = f (fn acc x) xs fns
   g [] = 0
   g (x : xs) = f x xs functions