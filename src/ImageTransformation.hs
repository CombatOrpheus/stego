-- | Embedding and extraction of encrypted messages in the blue channel of a
-- PPM image (least-significant-bit steganography).
module ImageTransformation
  ( hideEncryptedMessage
  , extractEncryptedMessage
  ) where

import BinaryOperation
import Data.Char (digitToInt)
import Data.Word (Word64)
import ToPixels (Pixel(..))

-- Width, in pixels/bits, of the length prefix written before the message.
sizePrefixBits :: Int
sizePrefixBits = 32

-- | Hide an encrypted message (as 64-bit integer blocks) plus its size in the
-- last bit of every blue channel.
hideEncryptedMessage :: [Pixel] -> [Integer] -> Integer -> [Pixel]
hideEncryptedMessage rgbValues messageInteger sizeImage =
  hidingMessage rgbValues (messageToBinary messageInteger sizeImage)

-- | Recover the encrypted 'Word64' blocks hidden in the pixel list: read the
-- 32-bit size prefix, then @size * 64@ further bits split into 64-bit chunks.
extractEncryptedMessage :: [Pixel] -> [Word64]
extractEncryptedMessage pixels =
  [ binaryToInteger chunk | chunk <- split64Bits messageBits ]
  where
    (sizePixels, messagePixels) = splitAt sizePrefixBits pixels
    messageSize = binaryToInt (rgbToMessageBinary sizePixels)
    messageBits = rgbToMessageBinary (take (messageSize * 64) messagePixels)

-- | Write the message bits into the blue channels, one bit per pixel.
hidingMessage :: [Pixel] -> BinaryString -> [Pixel]
hidingMessage [] (_:_) =
  error "The message is bigger than the values, asshole!"
hidingMessage rgbValues [] = rgbValues
hidingMessage ((RGB red green blue):rest) (currentBit:messageBit) =
  RGB red green (changeLastBit blue (digitToInt currentBit)) : hidingMessage rest messageBit
