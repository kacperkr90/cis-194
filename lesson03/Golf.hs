{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.Maybe

skips :: [a] -> [[a]]
skips [] = [[]]
skips list = map mapMaybeListToList $ skipsSafe list

takeNth :: Int -> [a] -> Maybe a
takeNth n list
    | n < 0 = headSafe list
    | otherwise = headSafe $ drop (n - 1) list

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x:_) = Just x

takeEveryNth :: [a] -> Int -> [Maybe a]
takeEveryNth [] _ = []
takeEveryNth _ 0 = []
takeEveryNth list n = takeNth n list : takeEveryNth (drop n list) n

type Index = Int
skipsSafe :: [a] -> [[Maybe a]]
skipsSafe list = map (takeEveryNth list) [1..(length list)]

mapMaybeListToList :: [Maybe a] -> [a]
mapMaybeListToList list = map fromJust $ filter isJust list