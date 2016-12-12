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

getTimestamp :: Parameters -> TimeStamp 
getTimestamp = read . head

drop2 :: [String] -> [String]
drop2 = drop 2

getMessageContent :: Parameters -> String
getMessageContent = unwords . drop 1

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = createEmptyNode m
insert m (Node _ (Unknown _) _) = createEmptyNode m
insert m@(LogMessage _ ct _) (Node l m2@(LogMessage _ pt _) r) 
  | ct < pt = case l of
    Leaf -> Node (createEmptyNode m) m2 r
    _    -> Node (insert m l) m2 r
  | otherwise = case r of
    Leaf -> Node l m2 (createEmptyNode m)
    _    -> Node l m2 (insert m r)


createEmptyNode :: LogMessage -> MessageTree
createEmptyNode lm = Node Leaf lm Leaf

build :: [LogMessage] -> MessageTree
build messages = buildTree messages Leaf 

buildTree :: [LogMessage] -> MessageTree -> MessageTree
buildTree [] t     = t
buildTree [x] t    = insert x t
buildTree (x:xs) t = buildTree xs (insert x t)

inOrder :: MessageTree -> [LogMessage]
inOrder (Node Leaf lm Leaf) = [lm]
inOrder (Node l lm r)       = inOrder l ++ [lm] ++ inOrder r
inOrder Leaf                = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = mapErrorMessagesToString . filterToSevereErrors . takeOnlyErrors . inOrder . build

takeOnlyErrors :: [LogMessage] -> [LogMessage]
takeOnlyErrors (lm@(LogMessage (Error _) _ _) : xs) = lm : takeOnlyErrors xs
takeOnlyErrors (_:xs) = takeOnlyErrors xs
takeOnlyErrors []     = []

filterToSevereErrors :: [LogMessage] -> [LogMessage]
filterToSevereErrors (lm@(LogMessage (Error serevity) _ _) : xs)
  | serevity > 50 = lm : filterToSevereErrors xs
  | otherwise     = filterToSevereErrors xs
filterToSevereErrors (_:xs) = filterToSevereErrors xs
filterToSevereErrors [] = []

mapErrorMessagesToString :: [LogMessage] -> [String]
mapErrorMessagesToString (x:xs) = mapErrorMessageToString x : mapErrorMessagesToString xs
mapErrorMessagesToString []     = []

mapErrorMessageToString :: LogMessage -> String
mapErrorMessageToString (LogMessage _ _ info) = info
mapErrorMessageToString _                     = ""

mapErrorMessageToString' :: LogMessage -> String
mapErrorMessageToString' (LogMessage errorType timeStamp info) = unwords [mapErrorTypeToString errorType, mapTimeStampToString timeStamp, info]
mapErrorMessageToString' _                                     = ""

mapErrorTypeToString :: MessageType -> String
mapErrorTypeToString (Error serevity) = unwords ["E", show serevity]
mapErrorTypeToString _                = ""

mapTimeStampToString :: TimeStamp -> String
mapTimeStampToString = show
