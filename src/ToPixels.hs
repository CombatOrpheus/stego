-- | PPM (P3) pixel handling: parsing the image body into 'Pixel's, reading
-- the hidden bits back out, and serialising pixels for writing.
module ToPixels
  ( Pixel(..)
  , getPixels
  , getBinaryMessage
  , rgbToMessageBinary
  , pixelListToString
  , getRGB
  ) where

import BinaryOperation (BinaryString, getLastBit)
import Data.Char (isDigit)

-- | A single RGB pixel with channel values in the usual 0..255 range.
data Pixel = RGB
  { red   :: Int
  , green :: Int
  , blue  :: Int
  }

instance Show Pixel where
  -- Each channel on its own line, matching the PPM ASCII body format.
  show (RGB r g b) = shows r '\n' (shows g '\n' (shows b '\n' ""))

-- | Serialise a pixel list back into PPM body text.
pixelListToString :: [Pixel] -> String
pixelListToString = concatMap show

-- | The hidden bit stream: the last bit of every blue channel, in order.
getBinaryMessage :: [Pixel] -> BinaryString
getBinaryMessage = map (getLastBit . blue)

-- | Same as 'getBinaryMessage'; kept because call sites that reconstruct an
-- embedded message read better under this name.
rgbToMessageBinary :: [Pixel] -> String
rgbToMessageBinary = getBinaryMessage

-- | Parse the numeric body of a PPM image into pixels (three values each).
getPixels :: String -> [Pixel]
getPixels image
  | length tokens `mod` 3 /= 0 = error "There's something wrong with this file!"
  | otherwise                  = toPixels tokens
  where
    tokens   = bodyNumbers image
    toPixels (r:g:b:rest) = RGB (read r) (read g) (read b) : toPixels rest
    toPixels _            = []

-- | Whitespace-separated tokens of the image body (everything after the
-- header).
bodyNumbers :: String -> [String]
bodyNumbers = words . unlines . dropHeader . lines

-- | Drop the P3 magic line, any @#@ comment lines, the dimension line and
-- the maxval line, leaving only the pixel data lines.
dropHeader :: [String] -> [String]
dropHeader ls = drop 2 afterComments
  where
    afterMagic = case ls of
      (m:ms) | take 2 m == "P3" -> ms
      _                        -> ls
    afterComments = dropWhile isComment afterMagic
    isComment l = not (null l) && head l == '#'

-- | Expose pixel channels as plain triples (handy for tests and debugging).
getRGB :: [Pixel] -> [(Int, Int, Int)]
getRGB = map (\p -> (red p, green p, blue p))
