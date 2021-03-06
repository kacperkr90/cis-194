{-# OPTIONS_GHC -Wall #-}

import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
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

-- Excercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert' Leaf

insert' :: a -> Tree a -> Tree a
insert' x Leaf = createLeaf x
insert' x (Node h Leaf v r) = Node (h + 1) (createLeaf x) v r
insert' x (Node h l v Leaf) = Node h l v (createLeaf x)
insert' x (Node h l v r)
  | hl < hr = Node h nl v r
  | hl > hr = Node h l v nr
  | hnl <= hr = Node h nl v r
  | otherwise = Node (hnr + 1) l v nr
  where
    hl = treeHeight l
    hr = treeHeight r 
    nl = insert' x l
    nr = insert' x r
    hnl = treeHeight nl
    hnr = treeHeight nr

createLeaf :: a -> Tree a
createLeaf x = Node 0 Leaf x Leaf

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node h _ _ _) = h

-- Excercise 3

xor :: [Bool] -> Bool
xor = odd . foldr incrementWhenTrue 0

incrementWhenTrue :: Bool -> Integer -> Integer
incrementWhenTrue True x = x + 1
incrementWhenTrue False x = x

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x -> (:) (f x)) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Excercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2 * x + 1) . minusLists . getRangeAndNotPrimesInThisRange

findDefinetlyNotPrimes :: Integer -> [Integer]
findDefinetlyNotPrimes = map sundaramSum . (\xs -> filter ((last xs >=) . sundaramSum) (cartProd xs xs)) . (\n -> [1 .. n])

sundaramSum :: (Integer, Integer) -> Integer
sundaramSum (x, y) = x + y + 2 * x * y

getRangeAndNotPrimesInThisRange :: Integer -> ([Integer], [Integer])
getRangeAndNotPrimesInThisRange n = ([1 .. n], findDefinetlyNotPrimes n)

minusLists :: ([Integer], [Integer]) -> [Integer]
minusLists (xs, ys) = xs \\ ys

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]	
