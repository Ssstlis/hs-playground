module FilterDisjoined where

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f g = filter (\v -> f v || g v)