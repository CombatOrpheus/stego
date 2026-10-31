-- | Low-level bit and binary-string helpers used by the steganography code.
--
-- A hidden message is represented as a 'BinaryString': a 32-bit length field
-- followed by one 64-bit chunk per encrypted character, written into the
-- least-significant bits of the image's blue channel.
module BinaryOperation
  ( changeLastBit
  , getLastBit
  , split64Bits
  , messageToBinary
  , binaryToInteger
  , binaryToInt
  , BinaryString
  ) where

import Data.Bits (testBit, xor)
import Data.Char (digitToInt, intToDigit)

-- | A sequence of @'0'@ and @'1'@ characters, most-significant bit first.
type BinaryString = String

-- | Overwrite the least-significant bit of a value with @bit@ (0 or 1).
changeLastBit :: Int -> Int -> Int
changeLastBit decimalValue bit
  | bit == 0    = setLastBit False
  | bit == 1    = setLastBit True
  | otherwise   = error "You should only change the last binary to 0 or 1, Dumbie"
  where
    -- Only touch the value when its last bit differs from the wanted state.
    setLastBit wanted
      | testBit decimalValue 0 == wanted = decimalValue
      | otherwise                        = decimalValue `xor` 1

-- | The last bit of a value, rendered as the character @'0'@ or @'1'@.
getLastBit :: Int -> Char
getLastBit value
  | testBit value 0 = '1'
  | otherwise       = '0'

-- | Encode a message as: 32 bits holding @messageSize@, then one padded
-- 64-bit chunk per encrypted word.
messageToBinary :: [Integer] -> Integer -> BinaryString
messageToBinary xs messageSize =
  padTo 32 (integerToBinary messageSize)
    ++ concatMap (padTo 64 . integerToBinary) xs

-- | Split a bit string into fixed-width 64-bit chunks.
-- The input length must be a multiple of 64.
split64Bits :: BinaryString -> [BinaryString]
split64Bits bits
  | null bits               = []
  | length bits `mod` 64 /= 0 = error "Errrrrrouuuuuuuuu!!!"
  | otherwise               = chunksOf 64 bits

chunksOf :: Int -> [a] -> [[a]]
chunksOf size [] = []
chunksOf size xs = take size xs : chunksOf size (drop size xs)

-- | Left-pad a bit string with zeros until it reaches @width@ characters.
padTo :: Int -> BinaryString -> BinaryString
padTo width binary
  | length binary >= width = binary
  | otherwise              = replicate (width - length binary) '0' ++ binary

-- | Binary representation of a non-negative integer (most-significant bit first).
integerToBinary :: Integer -> BinaryString
integerToBinary number
  | number < 2 = [intToDigit (fromInteger number)]
  | otherwise  = integerToBinary (number `quot` 2) ++ [intToDigit (fromInteger (number `rem` 2))]

-- | Parse a binary string as an 'Integer'.
binaryToInteger :: BinaryString -> Integer
binaryToInteger = binaryFold toInteger

-- | Parse a binary string as an 'Int'.
binaryToInt :: BinaryString -> Int
binaryToInt = binaryFold fromIntegral

-- | Shared left-to-right evaluation: @result = result * 2 + bit@.
-- This runs in linear time; the previous version that recomputed powers of
-- two at every position was quadratic in the length of the bit string.
binaryFold :: Num a => (Int -> a) -> BinaryString -> a
binaryFold _ "" = error "The binary string is empty!"
binaryFold conv bits
  | all (\c -> c == '0' || c == '1') bits =
      foldl (\acc c -> acc * 2 + conv (digitToInt c)) 0 bits
  | otherwise = error "The binary value is not valid"
