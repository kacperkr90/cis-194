{-# OPTIONS_GHC -Wall #-}

module JoinList(
  (+++),
  indexJ,
  testCase
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

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty     = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i l | i > (pred $ getListSize l) = Nothing
indexJ i (Append _ l1 l2)
  | i < n1 = indexJ i l1
  | otherwise = indexJ (i - n1) l2
  where
    n1 = getNumberOfIndices l1

getListSize :: (Sized m, Monoid m) => JoinList m a -> Int
getListSize = getSize . size . tag

getNumberOfIndices :: (Sized m, Monoid m) => JoinList m a -> Int
getNumberOfIndices (Single _ _) = 1
getNumberOfIndices l            = getListSize l

testCase :: JoinList Size Char
testCase = Append (Size 4) (Append (Size 3) (Single (Size 0) 'y') (Append (Size 2) (Single (Size 0) 'e') (Single (Size 0) 'a'))) (Single (Size 0) 'h')
