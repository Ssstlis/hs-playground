module GroupElems where

groupElems :: Eq a => [a] -> [[a]]
groupElems xs = go xs [] []
  where
    go [] [] acc1 = reverse acc1
    go [] acc0 acc1 = reverse (acc0 : acc1)
    go (head : tail) [] acc1 = go tail [head] acc1
    go (head : tail) (h : acc0t) acc1 | head == h = go tail (head : h : acc0t) acc1
                                      | otherwise = go tail [head] ((h : acc0t) : acc1)
