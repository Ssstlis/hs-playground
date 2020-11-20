module ISO where

import Control.Monad
import Data.Tuple
import Data.Void

import Data.Char

type ISO a b = (a -> b, b -> a)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

refl :: ISO a a
refl = (id, id)

symm :: ISO a b -> ISO b a
symm (f, s) = (s, f)

trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (liftM ab, liftM ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (f, g)
  where
    f (Left a) = Left (ab a)
    f (Right c) = Right (cd c)
    g (Left b) = Left (ba b)
    g (Right d) = Right (dc d)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\ac -> cd . ac . ba, \bd -> dc . bd . ab)

isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (ab, ba) = (f, g)
  where
    unb fc x = case fc (Just x) of
      Just r -> r
      Nothing -> case fc Nothing of
        Just y -> y
        Nothing -> error "impossible"
    f x = unb ab x
    g x = unb ba x

isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (f, g)
  where
    f (Right ()) = Left []
    f (Left x) = Left (() : x)
    g (Left []) = Right ()
    g (Left (_ : xs)) = Left xs

isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (symm, symm)

-- a = b -> c = d -> a * c = b * d
isoProd :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoProd = isoTuple

-- a = b -> c = d -> a + c = b + d
isoPlus :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoPlus = isoEither

-- a = b -> S a = S b
isoS :: ISO a b -> ISO (Maybe a) (Maybe b)
isoS = isoMaybe

-- a = b -> c = d -> c ^ a = d ^ b
isoPow :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoPow = isoFunc

-- a + b = b + a
plusComm :: ISO (Either a b) (Either b a)
plusComm = (f, g)
  where
    f (Left a) = Right a
    f (Right b) = Left b
    g (Left b) = Right b
    g (Right a) = Left a

-- a + b + c = a + (b + c)
plusAssoc :: ISO (Either (Either a b) c) (Either a (Either b c))
plusAssoc = (f, g)
  where
    f (Left (Left a)) = Left a
    f (Left (Right b)) = Right $ Left b
    f (Right c) = Right $ Right c
    g (Left a) = Left $ Left a
    g (Right (Left b)) = Left $ Right b
    g (Right (Right c)) = Right c

-- a * b = b * a
multComm :: ISO (a, b) (b, a)
multComm = (swap, swap)

-- a * b * c = a * (b * c)
multAssoc :: ISO ((a, b), c) (a, (b, c))
multAssoc = (\((a, b), c) -> (a, (b, c)), \(a, (b, c)) -> ((a, b), c))

-- dist :: a * (b + c) = a * b + a * c
dist :: ISO (a, (Either b c)) (Either (a, b) (a, c))
dist = (f, g)
  where
    f (a, Left b) = Left (a, b)
    f (a, Right c) = Right (a, c)
    g (Left (a, b)) = (a, Left b)
    g (Right (a, c)) = (a, Right c)

-- (c ^ b) ^ a = c ^ (a * b)
curryISO :: ISO (a -> b -> c) ((a, b) -> c)
curryISO = (uncurry, curry)

-- 1 = S O (we are using peano arithmetic)
-- https://en.wikipedia.org/wiki/Peano_axioms
one :: ISO () (Maybe Void)
one = (const Nothing, const ())

-- 2 = S (S O)
two :: ISO Bool (Maybe (Maybe Void))
two = (f, g)
  where
    f False = Nothing
    f True = Just Nothing
    g Nothing = False
    g (Just Nothing) = True

-- O + b = b
plusO :: ISO (Either Void b) b
plusO = (left, Right)
  where
    left (Left  x) = absurd x -- absurd :: Void -> a
    left (Right x) = x

-- S a + b = S (a + b)
plusS :: ISO (Either (Maybe a) b) (Maybe (Either a b))
plusS = (f, g)
  where
    f (Left Nothing) = Nothing
    f (Left (Just a)) = Just $ Left a
    f (Right b) = Just $ Right b
    g (Nothing) = Left Nothing
    g (Just (Left a)) = Left $ Just a
    g (Just (Right b)) = Right b

-- 1 + b = S b
plusSO :: ISO (Either () b) (Maybe b)
plusSO = isoPlus one refl `trans` plusS `trans` isoS plusO

-- O * a = O
multO :: ISO (Void, a) Void
multO = (\(x, a) -> x, \x -> (x, absurd x))

-- S a * b = b + a * b
multS :: ISO (Maybe a, b) (Either b (a, b))
multS = (f, g)
  where
    f (Nothing, b) = Left b
    f (Just a, b) = Right (a, b)
    g (Left b) = (Nothing, b)
    g (Right (a, b)) = (Just a, b)

-- 1 * b = b
multSO :: ISO ((), b) b
multSO =
  isoProd one refl `trans`
    multS `trans`
    isoPlus refl multO `trans`
    plusComm `trans`
    plusO

-- a ^ O = 1
powO :: ISO (Void -> a) ()
powO = (const (), const absurd)

tt :: (Maybe b -> a) -> (a, b -> a)
tt f = (f Nothing, \b -> f $ Just b)

-- a ^ (S b) = a * (a ^ b)
powS :: ISO (Maybe b -> a) (a, b -> a)
powS = (\fn -> (fn Nothing, \b -> fn $ Just b), g)
  where
    g (x, y) = fn
      where
        fn Nothing = x
        fn (Just a) = y a

-- a ^ 1 = a
-- Go the hard way (like multSO, plusSO)
-- to prove that you really get what is going on!
powSO :: ISO (() -> a) a
powSO = (\f -> f (), \a -> const a)


groupElems :: Eq a => [a] -> [[a]]
groupElems xs = go xs [] []
  where
    go [] [] acc1 = reverse acc1
    go [] acc0 acc1 = reverse (acc0 : acc1)
    go (head : tail) [] acc1 = go tail (head : []) acc1
    go (head : tail) (h : acc0t) acc1 | head == h = go tail (head : h : acc0t) acc1
                                      | otherwise = go tail (head : []) ((h : acc0t) : acc1)

foldWhile :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldWhile p f z a = inn a z
  where
    inn [] acc = acc
    inn (x : xs) acc = let new = (f acc x) in if (p new) then inn xs new else acc

