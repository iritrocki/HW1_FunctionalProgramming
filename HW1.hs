-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW1 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.

import Data.ByteString (count)
import Distribution.Simple.Utils (xargs)
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), curry, div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- ********* --

-- Section 1

-- ********* --

const :: a -> b -> a
const x _ = x

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) f g x = g $ f x

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate f z x y = f x y z

lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f y z x = f x y z

-- Generalizations of (.)
(.:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:) f g x y = f . g x y

(.:.) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.:.) f g x = f .: g x

(.::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::) f g x = f .:. g x

(.::.) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.::.) f g x = f .:: g x

-- How can we ever implement such a function!?
impossible :: a -> b
impossible = error "not possible to output value of unknown type"

-- ********* --

-- Section 2

-- ********* --

countDigits :: Integer -> Integer
countDigits x
  | x < 10 && x > (-10) = 1
  | otherwise = 1 + countDigits (x `div` 10)

toBinary :: Integer -> Integer
toBinary x
  | x == 1 || x == 0 = x
  | x < 0 = (-1) * toBinary (-x)
  | otherwise = 10 * toBinary (x `div` 2) + x `mod` 2

fromBinary :: Integer -> Integer
fromBinary x
  | x == 1 || x == 0 = x
  | x < 0 = (-1) * fromBinary (-x)
  | otherwise = 2 * fromBinary (x `div` 10) + x `mod` 10

isAbundant :: Integer -> Bool
isAbundant x
  | x < 0 = False
  | otherwise = sumDivisors x (x - 1) > x

sumDivisors :: Integer -> Integer -> Integer
sumDivisors _ 1 = 1
sumDivisors x y
  | x `mod` y == 0 = y + sumDivisors x (y - 1)
  | otherwise = sumDivisors x (y - 1)

rotateDigits :: Integer -> Integer
rotateDigits x
  | x < 0 = (-1) * rotateRight (-x)
  | otherwise = (x `mod` power 10 (countDigits x - 1)) * 10 + x `div` power 10 (countDigits x - 1)

rotateRight :: Integer -> Integer
rotateRight x = (x `div` 10) + (x `mod` 10) * power 10 (countDigits x - 1)

power :: Integer -> Integer -> Integer
power _ 0 = 1
power x y = x * power x (y - 1)

-- ********* --

-- Section 3

-- ********* --

type Generator a = (a -> a, a -> Bool, a)

nullGen :: Generator a -> Bool
nullGen (_, f, x) = not (f x)

lastGen :: Generator a -> a
lastGen (f, g, x)
  | nullGen (f, g, x) = x
  | otherwise = lastGen (f, g, f x)

lengthGen :: Generator a -> Int
lengthGen (f, g, x)
  | nullGen (f, g, x) = 0
  | otherwise = 1 + lengthGen (f, g, f x)

sumGen :: Generator Integer -> Integer
sumGen (f, g, x)
  | nullGen (f, g, x) = 0
  | otherwise = f x + sumGen (f, g, f x)

type Predicate a = a -> Bool

positives :: Generator Integer
positives = ((+ 1), const True, 0)

positivesUpTo10 :: Generator Integer
positivesUpTo10 = ((+ 1), (< 10), 0)

anyGen :: Predicate a -> Generator a -> Bool
anyGen p (f, g, x)
  | nullGen (f, g, x) = False
  | otherwise = p (f x) || anyGen p (f, g, f x)

allGen :: Predicate a -> Generator a -> Bool
allGen p (f, g, x) = not (anyGen (not . p) (f, g, x))

noneGen :: Predicate a -> Generator a -> Bool
noneGen p g = not (anyGen p g)

countGen :: Predicate a -> Generator a -> Int
countGen p (f, g, x)
  | noneGen p (f, g, x) = 0
  | p (f x) = 1 + countGen p (f, g, f x)
  | otherwise = countGen p (f, g, f x)

-- ********* --

-- Section 4

-- ********* --

isPrime :: Integer -> Bool
isPrime x
  | x < 2 = False
  | otherwise = not (anyGen (x `divisible`) ((+ 1), (< x - 1), 1))

divisible :: Integer -> Integer -> Bool
divisible x y = x `mod` y == 0

isSemiprime :: Integer -> Bool
isSemiprime x = anyGen (x `semiPrimePredicate`) ((+ 1), (<= x `div` 2), 1)

semiPrimePredicate :: Integer -> Integer -> Bool
semiPrimePredicate x y = divisible x y && isPrime y && isPrime (x `div` y)

goldbachPair :: Integer -> (Integer, Integer)
goldbachPair num = goldbachPairSearch num 2
  where
    goldbachPairSearch x y
      | isPrime y && isPrime (x - y) = (y, x - y)
      | otherwise = goldbachPairSearch x (y + 1)

goldbachPair' :: Integer -> (Integer, Integer)
goldbachPair' num = aux num num (0, 0)
  where
    aux x y (z, w)
      | y == 0 = (z, w)
      | isPrime y && isPrime (x - y) && y * (x - y) > z * w = aux x (y - 1) (y, x - y)
      | otherwise = aux x (y - 1) (z, w)

-- ***** --

-- Bonus

-- ***** --

isCircularPrime :: Integer -> Bool
isCircularPrime x = checkAllRotationsLeft x (countDigits x) && checkAllRotationsRight x (countDigits x)

checkAllRotationsLeft :: Integer -> Integer -> Bool
checkAllRotationsLeft x c
  | c == 0 = True
  | otherwise = isPrime x && checkAllRotationsLeft (rotateDigits x) (c - 1)

checkAllRotationsRight :: Integer -> Integer -> Bool
checkAllRotationsRight x c
  | c == 0 = True
  | otherwise = isPrime x && checkAllRotationsRight ((-1) * rotateDigits (-x)) (c - 1)
