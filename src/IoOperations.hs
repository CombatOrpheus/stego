module IoOperations where

import Data.Char
import Data.Word
import DES
import ImageTransformation
import ToPixels
import WordOperations

-- It reads the image
getImage :: FilePath -> IO String
getImage path
  | null path = error "The path is invalid\n"
  | otherwise = do
    image <- (readFile path)
    return (image)

-- It writes the final image, with the message hidde within
getStegnoKey :: FilePath -> String -> IO ()
getStegnoKey path values
  | null path = error "The path is invalid\n"
  | otherwise = do
    let image = values
    writeFile path image
    return ()

getImagePath :: IO String
getImagePath = do
  print "Path to Image:"
  image <- getLine
  return (image)

getKey :: IO String
getKey = do
  print "Encryption/Decryption Key:"
  key <- getLine
  return (key)

toNum :: [Int] -> Integer
toNum (x:xs)
  | null xs = toInteger x
  | otherwise = (toInteger x * 10 ^ (length xs)) + toNum xs

keyToNum :: String -> Integer
keyToNum number = toNum (map digitToInt number)

getMessage :: IO String
getMessage = do
  print "Message to embed:"
  message <- getLine
  return (message)

-- It returns the number of characters in the message
messageSize :: [Word64] -> Integer
messageSize message = toInteger (length message)

resolution :: String -> Integer
resolution image = horizontal * vertical
  where
    resolutionLine = last (init (lines (decapitation image)))
    size = words resolutionLine
    horizontal = read (head size) :: Integer
    vertical = read (head (tail size)) :: Integer

-- It checks if it's possible to hide the message within the image
messageToimage :: Integer -> Integer -> Bool
messageToimage image message
  | total > message = error "The message " False
  | otherwise = True
  where
    total = 32 + message * 64

decapitation :: String -> String
decapitation imageString = header (lines imageString)

-- It returns the original header of the image, to later write
header :: [String] -> String
header (xs:ys)
  | null xs = error "PPM P3 header is invalid"
  | elem '#' xs = xs ++ "\n" ++ (header ys)
  | elem ' ' xs = xs ++ "\n" ++ (head ys) ++ "\n"
  | otherwise = xs ++ "\n" ++ (header ys)

messageCheck :: String -> String -> Bool
messageCheck image message | total > imageS = False
                           | otherwise = True
  where
    total = 32 + ((length message) * 64)
    imageS =  read (show (resolution image)) :: Int

writeImage :: IO ()
writeImage = do  path <- getImagePath
                 image <- getImage path
                 message <- getMessage
                 if (messageCheck image message)
                 then(do initialKey <- getKey
                         let rgb = getPixels image
                             key = keyToNum initialKey
                             encryptedMessage = encryptMessage key message
                             imageHead = decapitation image
                             size = messageSize encryptedMessage
                             newRgb = hideEncryptedMessage rgb (word64ToInteger encryptedMessage) size
                             ppmRGB = pixelListToString newRgb
                             newPath = path ++ "1"
                         getStegnoKey newPath (imageHead ++ ppmRGB)
                      )
                 else (print "Image is too small: choose a bigger image or a smaller message.")

extractMessage :: IO ()
extractMessage = do path <- getImagePath
                    image <- getImage path
                    initialKey <- getKey
                    let rgb = getPixels image
                        message = word64ToInteger (extractEncryptedMessage rgb)
                        key = keyToNum initialKey
                        decryptedMessage = word64ToInt (decryptMessage key message)
                        finalMessage = map chr decryptedMessage
                    writeFile (path ++ ".txt") finalMessage
                    print finalMessage
