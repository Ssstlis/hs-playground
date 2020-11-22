module Sum3 where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 as bs cs = f [] as bs cs
  where
    f acc (a : xas) (b : xbs) (c : xcs) = f ((a + b + c) : acc) xas xbs xcs
    f acc [] (b : xbs) (c : xcs) = f ((b + c) : acc) [] xbs xcs
    f acc (b : xbs) [] (c : xcs) = f ((b + c) : acc) [] xbs xcs
    f acc (b : xbs) (c : xcs) [] = f ((b + c) : acc) [] xbs xcs
    f acc [] [] (c : xcs) = f (c : acc) [] [] xcs
    f acc [] (c : xcs) [] = f (c : acc) [] [] xcs
    f acc (c : xcs) [] [] = f (c : acc) [] [] xcs
    f acc [] [] [] = reverse acc
