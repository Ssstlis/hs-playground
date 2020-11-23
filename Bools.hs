module Bools where

orr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
orr f g = \v -> (f v) || (g v)

andr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andr f g = \v -> (f v) && (g v)

onr :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
onr c f g = \v -> c (f v) (g v)