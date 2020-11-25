module FoldingList where

foldList :: [Int] -> Int -> [Int]
foldList xs 0 = xs
foldList lst n = foldList g (n - 1)
  where
    (d, m) = divMod (length lst) 2
    (ss, ys) = splitAt (d + m) lst
    f ss []         = ss
    f (s:ss) (y:ys) = (s + y) : f ss ys
    g = f ss $ reverse ys

dm :: [a] -> (Int, Int)
dm xs = divMod (length xs) 2

spl :: [a] -> ([a], [a])
spl xs = let (d, m) = dm xs in splitAt (d + m) xs

zipWithMod :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithMod r xs ys = f xs $ reverse ys
  where
    f ss []         = ss
    f (s:ss) (y:ys) = r s y : f ss ys
