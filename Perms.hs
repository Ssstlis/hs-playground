module Perms where

import Data.List (nub)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = foldl (\acc r -> concatMap (ins r [] []) acc) [[x]] xs
  where
    ins e acc   pr []        = reverse (e : pr) : acc
    ins e [[]]  pr ss@(s:sf) = ins e [reverse (e : pr) ++ ss]         (s : pr) sf
    ins e acc   pr ss@(s:sf) = ins e ((reverse (e : pr) ++ ss) : acc) (s : pr) sf


permsU :: Eq a => [a] -> [[a]]
permsU = nub . perms
