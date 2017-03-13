-- Excercise 1

import Data.List

fib:: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x- 1) + fib (x- 2)

fibs1:: [Integer]
fibs1= map fib [0 .. ]

-- Excercise 2

data Stream = Cons Integer Stream
  deriving Show

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
