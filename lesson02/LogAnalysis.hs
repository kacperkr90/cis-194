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
    lmt  -> Node (insert m lmt) m2 r
  | otherwise = case l of
    Leaf -> Node l m2 (createEmptyNode m)
    rmt  -> Node l m2 (insert m rmt)


createEmptyNode :: LogMessage -> MessageTree
createEmptyNode lm = Node Leaf lm Leaf

build :: [LogMessage] -> MessageTree
build messages = buildTree messages Leaf 

buildTree :: [LogMessage] -> MessageTree -> MessageTree
buildTree [] t = t
buildTree [x] t = insert x t
buildTree (x:xs) t = buildTree xs (insert x t)

inOrder :: MessageTree -> [LogMessage]
inOrder (Node Leaf lm Leaf) = [lm]
inOrder (Node lt lm gt) = inOrder lt ++ [lm] ++ inOrder gt
inOrder Leaf = []
