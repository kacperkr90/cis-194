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
fibl (x0 : x1 : xs) y =  next : rest
  where
    next = x0 + x1
    rest = (x0 : x1 : xs)
