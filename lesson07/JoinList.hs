{-# OPTIONS_GHC -Wall #-}

module JoinList(
  (+++),
  indexJ,
  dropJ,
  takeJ,
  jl,
  jlToList,
  testIndexJ,
  testDropJ,
  testTakeJ
  ) where

import Sized
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
    n1 = getNumberOfIndices l1
indexJ _ _ = Nothing

testIndexJ :: Int -> Bool
testIndexJ i = (indexJ i jl) == (jlToList jl !!? i)

getListSize :: (Sized m, Monoid m) => JoinList m a -> Int
getListSize = getSize . size . tag

getNumberOfIndices :: (Sized m, Monoid m) => JoinList m a -> Int
getNumberOfIndices (Single _ _) = 1
getNumberOfIndices l            = getListSize l

jl :: JoinList Size Char
jl = Append (Size 4) (Append (Size 3) (Single (Size 0) 'y') (Append (Size 2) (Single (Size 0) 'e') (Single (Size 0) 'a'))) (Single (Size 0) 'h')

-- writing fold is overkill, but fun nonetheless :P

foldSomeJ :: (Sized b, Monoid b) => (b -> JoinList b a -> JoinList b a -> JoinList b a) -> (b -> JoinList b a -> JoinList b a -> JoinList b a) -> (JoinList b a -> JoinList b a) -> (JoinList b a -> JoinList b a) -> Int -> JoinList b a -> JoinList b a
foldSomeJ _ _ expD _ i l | i <= 0 = expD l
foldSomeJ fl fr expD expO i (Append m l1 l2)
  | i <= s1   = fl m (foldSomeJ fl fr expD expO i l1) l2
  | otherwise = fr m l1 (foldSomeJ fl fr expD expO (i - s1) l2)
  where
    s1 = getNumberOfIndices l1
foldSomeJ _ _ _ expO _ l = expO l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i l = foldSomeJ (\m l1 l2 -> Append m l1 l2) (\m _ l2 -> Append m Empty l2) id (\_ -> Empty) i l

testDropJ :: Int -> Bool
testDropJ n = jlToList (dropJ n jl) == drop n (jlToList jl)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i l = foldSomeJ (\m l1 _ -> Append m l1 Empty) (\m l1 l2 -> Append m l1 l2) (\_ -> Empty) id i l

testTakeJ :: Int -> Bool
testTakeJ n = jlToList (takeJ n jl) == take n (jlToList jl)
