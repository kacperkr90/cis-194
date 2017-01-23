{-# OPTIONS_GHC -Wall #-}

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x -> ((x - 2)*)) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate domainFun2

fun2'' :: Integer -> Integer
fun2'' = sum . filter even . takeWhile (>1) . iterate domainFun2'

domainFun2 :: Integer -> Integer
domainFun2 n = case even n of
  True -> n `div` 2
  False -> 3 * n + 1 

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ b = b

-- "Beautiful" point-free style domain function :D
domainFun2' :: Integer -> Integer
domainFun2' n = if' (even n) (n `div` 2) (3 * n + 1)
