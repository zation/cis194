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
