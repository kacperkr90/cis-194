-- Excercise 1

import Data.List

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
  show = unwords . take 20 . map show . streamToList

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
