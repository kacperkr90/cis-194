{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse content = map parseMessage (lines content) 

parseMessage :: String -> LogMessage
parseMessage = createLogMessage . words 

type Parameters = [String]

createLogMessage :: Parameters -> LogMessage
createLogMessage [] = emptyUnknownMessage 
createLogMessage params@(x : _)
  | isAnyKnownType x = createIdentifiedLogMessage (getMessageType params) params
  | otherwise        = Unknown (unwords params)
  where
      isAnyKnownType t = t `elem` ["I", "W", "E"]

emptyUnknownMessage :: LogMessage
emptyUnknownMessage = Unknown ""

getMessageType :: Parameters -> MessageType
getMessageType params = case take 2 params of
  ["E", y] -> Error (read y)
  ["I", _] -> Info
  _        -> Warning

createIdentifiedLogMessage :: MessageType -> Parameters -> LogMessage
createIdentifiedLogMessage (Error s) params = LogMessage (Error s) (getTimestamp (drop2 params)) (getMessageContent (drop2 params))
createIdentifiedLogMessage t (_:xs)         = LogMessage t (getTimestamp xs) (getMessageContent xs)
createIdentifiedLogMessage _ _              = emptyUnknownMessage

getTimestamp :: Parameters -> Int
getTimestamp = read . head

drop2 :: [String] -> [String]
drop2 = drop 2

getMessageContent :: Parameters -> String
getMessageContent = unwords . drop 1
