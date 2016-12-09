{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- parseMessage :: String -> LogMessage
-- parseMessage message = (words message)

emptyUnknownMessage :: LogMessage
emptyUnknownMessage = Unknown ""

type Parameters = [String]

metoda :: Parameters -> LogMessage
metoda [] = emptyUnknownMessage 
metoda params@(x : _)
	| isAnyKnownType x  = LogMessage (getMessageType (take 2 params)) (getTimestamp params) (getMessageContent params)  
	| otherwise 		= Unknown (unwords params) 
	where
		isAnyKnownType t = t `elem` ["I", "W", "E"]	 

getMessageType :: Parameters -> MessageType
getMessageType params = case (take 2 params) of
	["E", y] -> Error (read y)
	["I", _] -> Info
	_ -> Warning

getTimestamp :: Parameters -> Int
getTimestamp = read . head . (drop 2)

getMessageContent :: Parameters -> String
getMessageContent = unwords . (drop 3)


-- parseLogType :: [String] -> LogMessage
-- parseLogType (x:y:xs)
-- 	| isJust (read y) -> 

	-- LogMessage t 1 "test"
	-- Unknown ""