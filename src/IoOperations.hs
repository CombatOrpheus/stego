-- | Input/output glue: reading/writing PPM files, the small CLI prompts, and
-- the two end-to-end workflows (embed a message into an image, extract it
-- again).
module IoOperations
  ( getImage
  , getStegnoKey
  , getImagePath
  , getKey
  , getMessage
  , keyToNum
  , messageSize
  , resolution
  , decapitation
  , header
  , messageCheck
  , writeImage
  , extractMessage
  ) where

import Data.Char (chr, digitToInt)
import Data.Word (Word64)

import ChaCha20 (decryptMessage, encryptMessage)
import ImageTransformation (extractEncryptedMessage, hideEncryptedMessage)
import ToPixels (getPixels, pixelListToString)
import WordOperations (word64ToInteger, word64ToInt)

-- | Slurp an image file.
getImage :: FilePath -> IO String
getImage path
  | null path = error "The path is invalid\n"
  | otherwise = readFile path

-- | Write the final image, with the message hidden within.
getStegnoKey :: FilePath -> String -> IO ()
getStegnoKey path values
  | null path = error "The path is invalid\n"
  | otherwise = writeFile path values

getImagePath :: IO String
getImagePath = prompt "Path to Image:"

getKey :: IO String
getKey = prompt "Encryption/Decryption Key:"

getMessage :: IO String
getMessage = prompt "Message to embed:"

prompt :: String -> IO String
prompt label = do
  print label
  getLine

-- | Number of characters in the message (stored as a 32-bit size prefix).
messageSize :: [Word64] -> Integer
messageSize = toInteger . length

-- | Parse a decimal-digit key string into an 'Integer'.
keyToNum :: String -> Integer
keyToNum = foldl (\acc d -> acc * 10 + toInteger d) 0 . map digitToInt

-- | Total pixel count taken from the PPM dimension line (the last header line
-- before the maxval, i.e. second-to-last line of the stripped header).
resolution :: String -> Integer
resolution image = horizontal * vertical
  where
    sizeWords = words (last (init (lines (decapitation image))))
    horizontal = read (head sizeWords) :: Integer
    vertical   = read (head (tail sizeWords)) :: Integer

-- | Whether the image has enough pixels to carry @message@ characters
-- (one bit per pixel: 32 size bits plus 64 bits per character).
messageCheck :: String -> String -> Bool
messageCheck image message = total <= imagePixels
  where
    total = 32 + length message * 64
    imagePixels = fromIntegral (resolution image) :: Int

-- | The original header of the image, kept verbatim for writing back.
decapitation :: String -> String
decapitation imageString = header (lines imageString)

-- | Rebuild the header text (magic line, comments, dimensions, maxval) from
-- the raw lines: keep every line up to and including the first one that
-- contains a space (the @"W H"@ dimension line).
header :: [String] -> String
header [] = ""
header (xs:ys)
  | null xs        = error "PPM P3 header is invalid"
  | any (== '#') xs = xs ++ "\n" ++ header ys
  | any (== ' ') xs = xs ++ "\n" ++ head ys ++ "\n"
  | otherwise      = xs ++ "\n" ++ header ys

-- | Interactive workflow: embed an encrypted message into a user-chosen image.
writeImage :: IO ()
writeImage = do
  path <- getImagePath
  image <- getImage path
  message <- getMessage
  if messageCheck image message
    then do
      initialKey <- getKey
      let key = keyToNum initialKey
          encryptedMessage = encryptMessage key message
          rgb = getPixels image
          newRgb = hideEncryptedMessage rgb
                     (word64ToInteger encryptedMessage)
                     (messageSize encryptedMessage)
          imageHead = decapitation image
          ppmRGB = pixelListToString newRgb
      getStegnoKey (path ++ "1") (imageHead ++ ppmRGB)
    else
      print "Image is too small: choose a bigger image or a smaller message."

-- | Interactive workflow: extract and decrypt the message hidden in an image.
extractMessage :: IO ()
extractMessage = do
  path <- getImagePath
  image <- getImage path
  initialKey <- getKey
  let rgb = getPixels image
      message = word64ToInteger (extractEncryptedMessage rgb)
      key = keyToNum initialKey
      decryptedMessage = word64ToInt (decryptMessage key message)
      finalMessage = map chr decryptedMessage
  writeFile (path ++ ".txt") finalMessage
  print finalMessage
