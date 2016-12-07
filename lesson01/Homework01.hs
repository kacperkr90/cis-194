{-# OPTIONS_GHC -Wall #-}

mod10 :: Integer -> Integer
mod10 n = n `mod` 10

div10 :: Integer -> Integer
div10 n = n `div` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = mod10 n : toDigitsRev (div10 n)

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x] 


toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

multiply2 :: Integer -> Integer
multiply2 n = n * 2

doubleEveryTwo :: [Integer] -> [Integer]
doubleEveryTwo [] = []
doubleEveryTwo [x] = [x]
doubleEveryTwo (x:y:[]) = [x, multiply2 y]
doubleEveryTwo (x:y:zs) = x : (multiply2 y) : (doubleEveryTwo zs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleEveryTwo (reverse n))

digitize :: [Integer] -> [Integer]
digitize [] = []
digitize (x:xs) = (toDigitsRev x) ++ digitize xs 

validate :: Integer -> Bool
validate n = mod10 (sum (digitize (doubleEveryOther (toDigits n)))) == 0

-- HANOI

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n > 0 = hanoi (n - 1) a c b  ++ [(a, b)] ++ hanoi (n - 1) c b a 
  | otherwise = []
