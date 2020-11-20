module IsoString where

import Data.Char (ord)
import Data.Map (insert)
import qualified Data.Map as Map (empty,  lookup)

isomorph :: String -> String -> Bool
isomorph a b = length a == length b && f Map.empty a b && f Map.empty b a
  where
    f _ [] [] = True
    f _ [] (_ : _) = False
    f _ (_ : _) [] = False
    f mp (x : xs) (y : ys) = let
        xo = ord x
        yo = ord y
      in
        case Map.lookup xo mp of
          Just df -> (df == xo - yo) && f mp xs ys
          Nothing -> f (insert xo (xo - yo) mp) xs ys
