{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module JoinList(
  (+++),
  indexJ,
  dropJ,
  takeJ,
  jl,
  jlToList,
  testIndexJ,
  testDropJ,
  testTakeJ,
  scoreLine,
  Buffer(..),
  JoinList(..),
  Size(..),
  Score(..)
  ) where

import Sized
import Scrabble
import Buffer
-- data JoinListBasic a = Empty
--   | Single a
--   | Append (JoinListBasic a) (JoinListBasic a)

-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty          = []
-- jlbToList (Single a)     = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (t1 `mappend` t2) l1 l2
  where
    t1 = tag l1
    t2 = tag l2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _    = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0     = Just x
(_:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append _ l1 l2)
  | i < n1    = indexJ i l1
  | otherwise = indexJ (i - n1) l2
  where
    n1 = getListSize l1
indexJ _ _ = Nothing

testIndexJ :: Int -> Bool
testIndexJ i = (indexJ i jl) == (jlToList jl !!? i)

getListSize :: (Sized m, Monoid m) => JoinList m a -> Int
getListSize = getSize . size . tag

jl :: JoinList Size Char
jl = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')

-- writing fold is overkill , but fun nonetheless :P

foldSomeJ :: (Sized b, Monoid b) => (JoinList b a -> JoinList b a -> JoinList b a) -> (JoinList b a -> JoinList b a -> JoinList b a) -> (JoinList b a -> JoinList b a) -> (JoinList b a -> JoinList b a) -> Int -> JoinList b a -> JoinList b a
foldSomeJ _ _ expD _ i l | i <= 0 = expD l
foldSomeJ fl fr expD expO i (Append _ l1 l2)
  | i <= s1   = fl nl1 l2
  | otherwise = fr l1 nl2
  where
    s1 = getListSize l1
    nl1 = foldSomeJ fl fr expD expO i l1
    nl2 = foldSomeJ fl fr expD expO (i - s1) l2
foldSomeJ _ _ _ expO _ l = expO l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i l = foldSomeJ (\l1 l2 -> Append (tag l1 `mappend` tag l2) l1 l2) (\_ l2 -> Append (tag l2) Empty l2) id (\_ -> Empty) i l

testDropJ :: Int -> Bool
testDropJ n = jlToList (dropJ n jl) == drop n (jlToList jl)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i l = foldSomeJ (\l1 _ -> Append (tag l1) l1 Empty) (\l1 l2 -> Append (tag l1 `mappend` tag l2) l1 l2) (\_ -> Empty) id i l

testTakeJ :: Int -> Bool
testTakeJ n = jlToList (takeJ n jl) == take n (jlToList jl)

scoreLine :: String ->  JoinList Score String
scoreLine l = Single (scoreString l) l

scoreLine' :: String -> JoinList (Score, Size) String
scoreLine' s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ l) = l
  toString (Append _ l1 l2) = toString l1 ++ toString l2
  fromString s = foldl (+++) Empty $ map scoreLine' $ lines s
  line n b = indexJ n b
  numLines = getListSize
  value = getScore . fst . tag
  replaceLine n l b = takeJ n b +++ scoreLine' l +++ dropJ (n + 1) b