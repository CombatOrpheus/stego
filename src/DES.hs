module DES
  ( encryptMessage
  , decryptMessage
  ) where

import Codec.Encryption.DES
import Data.Word
import WordOperations

encryptMessage :: Integer -> [Char] -> [Word64]
encryptMessage key message =
  [encrypt encryptionKey a | a <- messageToWord64 message]
  where
    encryptionKey = read (show key) :: Word64

decryptMessage :: Integer -> [Integer] -> [Word64]
decryptMessage key message =
  [decrypt decryptionKey a | a <- integerToWord64 message]
  where
    decryptionKey = read (show key) :: Word64
