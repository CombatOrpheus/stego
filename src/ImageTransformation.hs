module ImageTransformation ( hideEncryptedMessage
                            ,extractEncryptedMessage
                           ) where

import BinaryOperation
import Data.Char
import Data.Word
import ToPixels

-- It takes a encrypted message and the rgb values and hide the message on the rgb
hideEncryptedMessage :: [Pixel] -> [Integer] -> Integer -> [Pixel]
hideEncryptedMessage rgbValues messageInteger sizeImage = hidingMessage  rgbValues (messageToBinary messageInteger sizeImage)

-- It receives all the image's pixels and then returns the encrypted pmessage
extractEncryptedMessage :: [Pixel] -> [Word64]
extractEncryptedMessage pixels = [read (show $ binaryToInteger char64) :: Word64 | char64 <- split64Bits $ rgbToMessageBinary messagePixels]
                               where messageSize = binaryToInt $ rgbToMessageBinary sizePixels
                                     messagePixels = take (messageSize * 64) (drop 32 pixels)
                                     sizePixels = take 32 pixels

-- Hidding the binary message on a list with rgb values
hidingMessage :: [Pixel] -> BinaryString -> [Pixel]
hidingMessage [] [] = []
hidingMessage [] xs = error $ "The message is bigger than the values, asshole!" ++ xs
hidingMessage rgbValues [] = rgbValues
hidingMessage ((RGB red green blue):rgbValues) (currentBit:messageBit) = newRGB : hidingMessage rgbValues messageBit
                                                                        where newRGB = RGB red green newBlue
                                                                              newBlue = changeLastBit  blue (digitToInt currentBit)
