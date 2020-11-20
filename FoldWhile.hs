module FoldWhile where

foldWhile :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldWhile p f z a = folder a z
  where
    folder [] acc = acc
    folder (x : xs) acc = let new = f acc x in if p new then folder xs new else acc

