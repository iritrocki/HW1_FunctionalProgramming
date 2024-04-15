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
  | x == 1 = x
  | x == 0 = x
  | x < 0 = (-1) * toBinary (-x)
  | otherwise = 10 * toBinary (x `div` 2) + x `mod` 2

fromBinary :: Integer -> Integer
fromBinary = undefined

isAbundant :: Integer -> Bool
isAbundant = undefined

rotateDigits :: Integer -> Integer
rotateDigits = undefined

-- ********* --

-- Section 3

-- ********* --

type Generator a = (a -> a, a -> Bool, a)

nullGen :: Generator a -> Bool
nullGen = undefined

lastGen :: Generator a -> a
lastGen = undefined

lengthGen :: Generator a -> Int
lengthGen = undefined

sumGen :: Generator Integer -> Integer
sumGen = undefined

type Predicate a = a -> Bool

anyGen :: Predicate a -> Generator a -> Bool
anyGen = undefined

allGen :: Predicate a -> Generator a -> Bool
allGen = undefined

noneGen :: Predicate a -> Generator a -> Bool
noneGen = undefined

countGen :: Predicate a -> Generator a -> Int
countGen = undefined

-- ********* --

-- Section 4

-- ********* --

isPrime :: Integer -> Bool
isPrime = undefined

isSemiprime :: Integer -> Bool
isSemiprime = undefined

goldbachPair :: Integer -> (Integer, Integer)
goldbachPair = undefined

goldbachPair' :: Integer -> (Integer, Integer)
goldbachPair' = undefined

-- ***** --

-- Bonus

-- ***** --

isCircularPrime :: Integer -> Bool
-- If you choose the implement this function, replace this with the actual implementation
isCircularPrime = undefined
