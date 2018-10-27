module WordOperations where

import Data.Word
import Data.Char

messageToWord64 :: [Char] -> [Word64]
messageToWord64 xs = [read $ show (ord a) :: Word64 | a <- xs]

integerToWord64 :: [Integer] -> [Word64]
integerToWord64 message = [read $ (show a) :: Word64 | a <- message]

word64ToInteger :: [Word64] -> [Integer]
word64ToInteger list = map toInteger list

word64ToInt :: [Word64] -> [Int]
word64ToInt list = [read $ (show a) :: Int | a <- list]
