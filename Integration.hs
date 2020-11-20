module Integration where

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper 0.0 9999
  where
    h = (b - a) / 10000
    helper acc 0 = h * (((f a + f b) / 2) + acc)
    helper acc n = helper (acc + f (a + (h * n))) (n - 1)