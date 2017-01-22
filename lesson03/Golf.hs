{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.Maybe
import Data.List

-- EXCERCISE 1

skips :: [a] -> [[a]]
skips []   = [[]]
skips list = map mapMaybeListToList $ skipsSafe list

takeNth :: Int -> [a] -> Maybe a
takeNth n list
  | n < 0     = headSafe list
  | otherwise = headSafe $ drop (n - 1) list

headSafe :: [a] -> Maybe a
headSafe []    = Nothing
headSafe (x:_) = Just x

takeEveryNth :: [a] -> Int -> [Maybe a]
takeEveryNth [] _   = []
takeEveryNth _ 0    = []
takeEveryNth list n = takeNth n list : takeEveryNth (drop n list) n

-- It requires 'a' to be instance of Eq class and it won't work on lists with non-unique elements. 
-- But it's cool and short :P
takeEveryNth2 :: Eq a => [a] -> Int -> [a]
takeEveryNth2 list n = filter (isDividedBy n . fromJust . flip elemIndex list) list

isDividedBy :: Int -> Int -> Bool
isDividedBy d n = n `mod` d == 0

type Index = Int
skipsSafe :: [a] -> [[Maybe a]]
skipsSafe list = map (takeEveryNth list) [1..(length list)]

mapMaybeListToList :: [Maybe a] -> [a]
mapMaybeListToList list = map fromJust $ filter isJust list

test :: Char -> Bool
test = isDividedBy 2 . fromJust . flip elemIndex "Hello!"

-- EXCERCISE 2

localMaxima :: [Integer] -> [Integer]
localMaxima = map toLocalMaxima . filter hasLocalMaxima . listOfLists

listOfLists :: [Integer] -> [(Integer, Integer, Integer)]
listOfLists (x:xs@(y:z:_)) = (x,y,z) : listOfLists xs
listOfLists _ = []

hasLocalMaxima :: (Integer, Integer, Integer) -> Bool
hasLocalMaxima (x, y, z) = max x y == y && max y z == y

toLocalMaxima :: (Integer, Integer, Integer) -> Integer
toLocalMaxima (_, y, _) = y

-- EXCERCISE 3
type Count = Int
type ValueToFind = Integer

histogram :: [Integer] -> String
histogram x = unlines $ reverse $ drawHistogramLegend : drawHistogramAxis : map (drawHistogramLevel x) [1 .. (findMaxNumberOfRepetitions x)]

countValueRepetitions :: [Integer] -> ValueToFind -> Count
countValueRepetitions [] _ = 0
countValueRepetitions list a = length $ filter (==a) list 

isValueRepeatedAtLeastNthTimes :: [Integer] -> Count -> ValueToFind -> Bool
isValueRepeatedAtLeastNthTimes list n a = n <= countValueRepetitions list a

drawHistogramCell :: [Integer] -> Count -> ValueToFind -> Char
drawHistogramCell [] _ _ = ' '
drawHistogramCell list n a = case isValueRepeatedAtLeastNthTimes list n a of
  True -> '*'
  False -> ' '

drawHistogramLevel :: [Integer] -> Count -> String
drawHistogramLevel list n = map (drawHistogramCell list n) [0..9]

findMaxNumberOfRepetitions :: [Integer] -> Int
findMaxNumberOfRepetitions list = maximum $ map length (group $ sort list)

drawHistogramAxis :: String
drawHistogramAxis = replicate 10 '='

drawHistogramLegend :: String
drawHistogramLegend = "0123456789"