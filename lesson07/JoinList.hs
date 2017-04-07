{-# OPTIONS_GHC -Wall #-}

module JoinList(
  (+++),
  indexJ
  ) where

import Sized
import Data.Maybe
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
indexJ i (Single _ a)
  | i > 0  = Nothing
  | otherwise = Just a
indexJ i (Append m _ _) | i > (pred $ getSize $ size m) = Nothing
indexJ i (Append _ l1 l2)
  | isNothing a1 = a2
  | otherwise = a1
  where
    a1 = indexJ i l1
    a2 = indexJ i l2
