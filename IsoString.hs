module IsoString where

import Data.Char (ord)
import Data.Map (Map, insert)
import qualified Data.Map as Map

isomorph :: String -> String -> Bool
isomorph a b = length a == length b && (f (Map.empty :: Map Int Int) a b)&& (f (Map.empty :: Map Int Int) b a)
  where
    f map [] [] = True
    f map (x : xs) (y : ys) = let
        xo = ord x
        yo = ord y
      in
        case Map.lookup xo map of
          Just df -> if (df == xo - yo) then f map xs ys else False
          Nothing -> f (Map.insert xo (xo - yo) map) xs ys
