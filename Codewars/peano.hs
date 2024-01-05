{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- module PC where

import Data.Function (on)
import Data.List
import Distribution.Simple.Utils (xargs)

type ISO a b = (a -> b, b -> a)

-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (convertab, convertba)
  where
    convertab f x y = ab (f (ba x) (ba y))
    convertba f x y = ba (f (ab x) (ab y))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natural Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natural Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r = successor $ (l `minus` r) `divide` r

  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} (Nat n) => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} (Nat n) => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} (Nat n) => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} (Nat n) => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natural Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero :: Peano
  zero = O
  successor :: Peano -> Peano
  successor = S
  plus :: Peano -> Peano -> Peano
  plus O a = a
  plus (S a) b = S (plus a b)
  nat :: a -> (Peano -> a) -> Peano -> a
  nat x f O = x
  nat x f (S a) = f a
  iter :: a -> (a -> a) -> Peano -> a
  iter x f O = x
  iter x f (S a) = iter (f x) f a
  minus :: Peano -> Peano -> Peano
  minus O _ = O
  minus (S a) (S b) = minus a b
  mult :: Peano -> Peano -> Peano
  mult O _ = O
  mult _ O = O
  mult a (S b) = plus a $ mult a b
  pow :: Peano -> Peano -> Peano
  pow _ O = successor zero
  pow a (S b) = mult a $ pow a b

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (),
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
  zero :: [()]
  zero = []
  successor :: [()] -> [()]
  successor = (() : )
  plus :: [()] -> [()] -> [()]
  plus [] a = a
  plus (() : a) b = () : plus a b
  nat :: a -> ([()] -> a) -> [()] -> a
  nat x f [] = x
  nat x f (() : a) = f a
  iter :: a -> (a -> a) -> [()] -> a
  iter x f [] = x
  iter x f (() : a) = iter (f x) f a
  minus :: [()] -> [()] -> [()]
  minus [] _ = []
  minus (() : a) (() : b) = minus a b
  mult :: [()] -> [()] -> [()]
  mult [] _ = []
  mult _ [] = []
  mult a  (() : b) = plus a $ mult a b
  pow :: [()] -> [()] -> [()]
  pow _ [] = successor zero
  pow a (() : b) = mult a $ pow a b
-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott {runScott :: forall a. a -> (Scott -> a) -> a}

instance Nat Scott where
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  zero :: Scott
  zero = Scott const
  successor :: Scott -> Scott
  successor n = Scott $ \z s -> s n
  nat :: a -> (Scott -> a) -> Scott -> a
  nat x f s = runScott s x f
  iter :: a -> (a -> a) -> Scott -> a
  iter x f s = iter (f x) f (Scott $ \z s' -> s' s)
  plus :: Scott -> Scott -> Scott
  plus = substR (liftISO2 isoP) plus
  minus :: Scott -> Scott -> Scott
  minus = substR (liftISO2 isoP) minus
  mult :: Scott -> Scott -> Scott
  mult = substR (liftISO2 isoP) mult
  pow :: Scott -> Scott -> Scott
  pow = substR (liftISO2 isoP) pow

-- Or from induction!
newtype Church = Church {runChurch :: forall a. (a -> a) -> a -> a}

instance Nat Church where
  zero :: Church
  zero = undefined
  successor :: Church -> Church
  successor = undefined
  nat :: a -> (Church -> a) -> Church -> a
  nat = undefined
  iter :: a -> (a -> a) -> Church -> a
  iter = undefined
  plus :: Church -> Church -> Church
  plus = undefined
  minus :: Church -> Church -> Church
  minus = undefined
  mult :: Church -> Church -> Church
  mult = undefined
  pow :: Church -> Church -> Church
  pow = undefined


-- Try to implement the calculation (except minus) in the primitive way.
-- Implement them by constructing Church explicitly.
-- So plus should not use successor,
-- mult should not use plus,
-- exp should not use mult.