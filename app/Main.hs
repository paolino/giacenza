{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Main where

import Lib
import Turtle
import Streaming
import Protolude hiding (FilePath)

parser :: Parser (FilePath, Integer)
parser =
  (,) <$> argPath "dir"  "The directory with the statements"
    <*> argInteger "year"  "The year for the giacenza"

main :: IO ()
main = do
  (dir, year) <- options "Giacenza media" parser
  v :> n <- giacenza (encodeString dir) $ Year year
  print n 
  print v 

