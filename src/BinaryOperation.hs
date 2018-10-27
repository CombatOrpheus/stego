module BinaryOperation
  ( changeLastBit
  , getLastBit
  , split64Bits
  , messageToBinary
  , binaryToInteger
  , binaryToInt
  , BinaryString
  ) where

import Data.Bits
import Data.Char

type BinaryString = String

-- It verifies the last bit and change it to 1 or 0
changeLastBit :: Int -> Int -> Int
changeLastBit decimalValue 0
  | testBit decimalValue 0 = xor decimalValue 1
  | otherwise = decimalValue
changeLastBit decimalValue 1
  | testBit decimalValue 0 = decimalValue
  | otherwise = xor decimalValue 1
changeLastBit decimalValue _ =
  error "You should only change the last binary to 0 or 1, Dumbie"

-- It transforms the mensage to Binary
messageToBinary :: [Integer] -> Integer -> BinaryString
messageToBinary xs messageSize = toList (binarySize : messageToBinary' xs)
                             where binarySize = completeBits (integerToBinary messageSize) 32

messageToBinary' :: [Integer] -> [BinaryString]
messageToBinary' messageCryp = [completeBits (integerToBinary charEncrypted) 64 | charEncrypted <- messageCryp]

toList :: [[a]] -> [a]
toList [] = []
toList ((x:[]):ys) = x : toList ys
toList ([]:ys) = toList ys
toList ((x:xs):ys) = x : toList (xs : ys)

completeBits :: BinaryString -> Integer -> [Char]
completeBits binary maxLength
  | toInteger (length binary)   < maxLength = completeBits newBinary maxLength
  | otherwise = binary
  where
    newBinary = '0' : binary

integerToBinary :: Integer -> BinaryString
integerToBinary 0 = "0"
integerToBinary 1 = "1"
integerToBinary number = (integerToBinary $ quot number 2) ++ (show (mod number 2))

getLastBit :: Int -> Char
getLastBit value
  | testBit value 0 = '1'
  | otherwise = '0'

binaryToInteger :: BinaryString -> Integer
binaryToInteger "" = error "The binary string is empty!"
binaryToInteger bitsValue = binaryToInteger' bitsValue (toInteger $ (length bitsValue) - 1)

binaryToInteger' :: BinaryString -> Integer -> Integer
binaryToInteger' [] _ = error "The binary value is not valid"
binaryToInteger' [onlyBit] 0 = toInteger ((digitToInt onlyBit) * (2 ^ 0))
binaryToInteger' (fstBit:bitValue) index =
       toInteger ((digitToInt fstBit) * (2 ^ index)) + binaryToInteger' bitValue (toInteger (index - 1))

binaryToInt :: BinaryString -> Int
binaryToInt "" = error "The binary string is empty!"
binaryToInt bitsValue = binaryToInt' bitsValue ((length bitsValue) - 1)

binaryToInt' ::  BinaryString -> Int -> Int
binaryToInt' [] _ = error "The binary value is not valid"
binaryToInt' [onlyBit] 0 = (digitToInt onlyBit) * (2 ^ 0)
binaryToInt' (fstBit:bitValue) index = ((digitToInt fstBit) * (2 ^ index)) + binaryToInt' bitValue (index - 1)

-- Ex. "1001001010000101" -> ["10010010","10000101"]
split64Bits :: BinaryString -> [BinaryString]
split64Bits "" = []
split64Bits word | mod (length word) 64 == 0 = take 64 word : split64Bits (drop 64 word)
                 | otherwise = error "Errrrrrouuuuuuuuu!!!"
