module Perms where

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = go xs [[x]]
  where
    go []       acc = acc
    go (r : rs) acc = go rs $ concatMap (ins r [] []) acc
  
    ins e acc   pr []        = reverse (e : pr) : acc
    ins e [[]]  pr ss@(s:sf) = ins e [reverse (e : pr) ++ ss]         (s : pr) sf
    ins e acc   pr ss@(s:sf) = ins e ((reverse (e : pr) ++ ss) : acc) (s : pr) sf
