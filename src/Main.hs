module Main where

import System.Environment

import DES
import Data.Char
import ImageTransformation
import IoOperations
import ToPixels
import WordOperations

main :: IO ()
main = do
  print "Desired Operation: [I]nsert / [E]xtract message:"
  operation <- getLine
  if (operation == "I")
    then (writeImage)
    else if (operation == "E")
           then (extractMessage)
           else (print "Wrong Operation")
  return ()
