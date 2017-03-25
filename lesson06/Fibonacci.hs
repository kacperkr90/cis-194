{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- Excercise 1

import Data.List
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Maybe (fromJust, isJust)

fib:: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x- 1) + fib (x- 2)

fibs1:: [Integer]
fibs1= map fib [0 .. ]

-- Excercise 2
-- first try, unfortunately a bad one. List causes infinite evaluation even with take 1 fibs2l
fibs2l:: [Integer]
fibs2l= reverse $ foldl' fibl [] [0 .. ]

fibl :: [Integer] -> Integer -> [Integer]
fibl [] x             = [x]
fibl [x] y            = [x, y]
fibl (x0 : x1 : xs) _ =  next : rest
  where
    next = x0 + x1
    rest = (x0 : x1 : xs)

fibs2' :: [[Integer]]
fibs2' =  iterate fib2 [1, 0]

fib2:: [Integer] -> [Integer]
fib2 []             = []
fib2 [x]            = [x]
fib2 (x0 : x1 : xs) =  next : rest
  where
    next = x0 + x1
    rest = (x0 : x1 : xs)

-- after checking some hints :(

-- iterate
fibsIter:: [Integer]
fibsIter= map fst $ iterate fib' (0, 1)
  where
    fib' (a, b) = (b, a + b)

-- zipWith
fibsZipWith :: [Integer]
fibsZipWith= 0 : 1 : zipWith (+) fibsZipWith (tail fibsZipWith)

-- scanl
fibsScanl :: [Integer]
fibsScanl = map head $ scanl fibl [0, 1] [2 ..]

-- Excercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a stream) = a : streamToList stream

instance Show a => Show (Stream a) where
  show = unwords . take 22 . map show . streamToList

-- Excercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap expr (Cons a stream) = Cons (expr a) (streamMap expr stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed expr a = Cons a (streamFromSeed expr (expr a))

-- Excercise 5

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a s1) s2 = Cons a (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = interleaveStreams zeros a001511Seq
  where
    zeros         = streamRepeat 0
    oneToInfinity = streamFromSeed succ 1
    a001511Seq    = streamMap (succ . valueOrMinusOne . indexOfOneFromLeft . toBinaryRepresentation) oneToInfinity

toBinaryRepresentation :: Integer -> String
toBinaryRepresentation x = showIntAtBase 2 intToDigit x ""

indexOfOneFromLeft :: String -> Maybe Int
indexOfOneFromLeft x = elemIndex '1' (reverse x)

valueOrMinusOne :: Maybe Int -> Integer
valueOrMinusOne x
  | isJust x = toInteger $ fromJust x
  | otherwise = -1

z :: Stream Integer
z = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n                  = Cons n (streamRepeat 0)
  negate s                       = streamMap negate s
  (Cons a sa) + (Cons b sb)      = Cons (a + b) (sa + sb)
  (Cons a0 sa) * s2@(Cons b0 sb) = Cons (a0 * b0) (a0B' + a'b)
    where
      a0B' = streamMap (a0*) sb
      a'b  = sa * s2

instance Fractional (Stream Integer) where
  s1@(Cons a0 sa) / s2@(Cons b0 sb) =  Cons (a0 `div` b0) (sa - q * sb)
    where
      q = s1 / s2

fibs3 :: Stream Integer
fibs3 = z / (1 - z - z^2)

data Matrix = M (Integer, Integer) (Integer, Integer)

            -- [ b00, b01 ]
            -- [ b10, b11
-- [ a00, a01 ]
-- [ a10, a11 ]

instance Show Matrix where
  show (M (a00, a01) (a10, a11)) = "[ " ++ show a00 ++ " " ++ show a01 ++ " ]\n" ++ "[ " ++ show a10 ++ " " ++ show a11 ++ " ]"

instance Num Matrix where
  fromInteger n                                     = M (n, n) (n, n)
  M (a00, a01) (a10, a11) + M (b00, b01) (b10, b11) = M (c00, c01) (c10, c11)
    where
      c00 = a00 + b00
      c01 = a01 + b01
      c10 = a10 + b10
      c11 = a11 + b11
  M (a00, a01) (a10, a11) * M (b00, b01) (b10, b11) = M (c00, c01) (c10, c11)
    where
      c00 = a00 * b00 + a01 * b10
      c01 = a00 * b01 + a01 * b11
      c10 = a10 * b00 + a11 * b10
      c11 = a10 * b01 + a11 * b11
  negate a = a * (-1)

f :: Matrix
f = M (1, 1) (1, 0)

fib4:: Integer -> Integer
fib4 0 = 0
fib4 n = res
  where
    fn = f ^ n
    M (_, res) _ = fn
