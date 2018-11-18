module Homework2 where

import Log

parseMessage :: String -> LogMessage
parseMessage message = case words message of
  "E":severity:timeStamp:content -> LogMessage (Error (read severity::Int)) (read timeStamp::Int) (unwords content)
  "I":timeStamp:content -> LogMessage Info (read timeStamp::Int) (unwords content)
  "W":timeStamp:content -> LogMessage Warning (read timeStamp::Int) (unwords content)
  content -> Unknown (unwords content)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert newLogMessage@(LogMessage _ newTimeStamp _) (Node left logMessage@(LogMessage _ timeStamp _) right)
  | newTimeStamp > timeStamp = Node left logMessage (Node Leaf newLogMessage Leaf)
  | otherwise = Node (Node Leaf newLogMessage Leaf) logMessage right
