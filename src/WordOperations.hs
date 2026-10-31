-- | Conversions between the numeric types used across the tool.
--
-- Characters travel through the pipeline as 'Word64' blocks, but the image
-- layer and the CLI speak 'Int' and 'Integer', so this module provides the
-- direct, total conversions between them (no more @read . show@ round-trips).
module WordOperations
  ( messageToWord64
  , integerToWord64
  , word64ToInteger
  , word64ToInt
  ) where

import Data.Char (ord)
import Data.Word (Word64)

-- | One 'Word64' per character of the message.
messageToWord64 :: [Char] -> [Word64]
messageToWord64 = map (fromIntegral . ord)

-- | Reinterpret encrypted 'Integer' blocks as 'Word64' values.
integerToWord64 :: [Integer] -> [Word64]
integerToWord64 = map fromIntegral

word64ToInteger :: [Word64] -> [Integer]
word64ToInteger = map fromIntegral

-- | Narrow 'Word64' blocks back to 'Int' (used to recover character codes).
word64ToInt :: [Word64] -> [Int]
word64ToInt = map fromIntegral
