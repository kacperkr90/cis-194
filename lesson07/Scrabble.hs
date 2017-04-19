{-# OPTIONS_GHC -Wall #-}

module Scrabble(
  Score(..),
  score,
  scoreString
  ) where

import qualified Data.Map as Map
import Data.Char
import Data.Maybe

newtype Score = Score Int
  deriving (Eq, Show, Ord)

instance Monoid Score where
  mempty = Score 0
  Score a `mappend` Score b = Score (a + b)

letters :: [Char]
letters = ['A' .. 'Z']

scores :: [Int]
scores = [1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10]

lettersToScores :: Map.Map Char Score
lettersToScores = Map.fromList $ map (\(k, v) -> (k, Score v)) $ zip letters scores

score :: Char -> Score
score c
  | isJust val = fromJust val
  | otherwise = Score 0
  where
    val = Map.lookup (toUpper c) lettersToScores

scoreString :: String -> Score
scoreString = foldl (mappend) mempty . map score
