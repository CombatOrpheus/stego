module ToPixels where

import BinaryOperation
import Data.Char
import Data.Word

data Pixel = RGB
  { red :: Int
  , green :: Int
  , blue :: Int
  }

instance Show Pixel where
    show (RGB red green blue) =
        (show red) ++ "\n" ++ (show green) ++ "\n" ++ (show blue) ++ "\n"

pixelListToString :: [Pixel] -> String
pixelListToString [] = []
pixelListToString (a:as) = show a ++ pixelListToString as

-- It takes the RGB values of the image and then it returns the message in the binary form
getBinaryMessage :: [Pixel] -> BinaryString
getBinaryMessage [] = ""
getBinaryMessage ((RGB _ _ blue):rgbValues) = (getLastBit blue) : getBinaryMessage rgbValues

rgbValues :: (String, String, String) -> Pixel
rgbValues (xs, ys, zs) = RGB (read xs :: Int) (read ys :: Int) (read zs :: Int)

-- Returns a list of Pixels
getPixels :: String -> [Pixel]
getPixels image = getPixels' $ headerRemover (numbers image)

-- Transform a Pixel list into a BinaryString represantation
rgbToMessageBinary :: [Pixel] -> String
rgbToMessageBinary [] = []
rgbToMessageBinary [(RGB _ _ blue)] = [getLastBit blue]
rgbToMessageBinary ((RGB _ _ blue):rgbValues) = (getLastBit blue) : rgbToMessageBinary rgbValues

getPixels' :: [String] -> [Pixel]
getPixels' [] = []
getPixels' (xs:[]) = error "There's something wrong with this file!"
getPixels' (xs:ys:[]) = error "There's something wrong with this file!"
getPixels' (xs:ys:zs:as) = rgbValues (xs, ys, zs) : getPixels' as

getRGB :: [Pixel] -> [(Int, Int, Int)]
getRGB (x:xs) = (red x, green x, blue x) : getRGB xs

--It separates the String in a list of Strings.
numbers :: String -> [String]
numbers [] = []
numbers l = take n l : numbers (drop (n + 1) l)
  where
    n = lfSeparation l

--It returns the position of the Line Breaks
lfSeparation :: String -> Int
lfSeparation [] = 0
lfSeparation (x:xs)
  | x /= '\n' = 1 + lfSeparation xs
  | otherwise = 0

-- It removes the header lines comment (Px, Creator comments and resolution)
headerRemover :: [String] -> [String]
headerRemover (xs:ys)
  | null xs = error "No Image"
  | elem '#' xs = drop 2 ys
  | elem ' ' xs = tail ys
  | otherwise = headerRemover ys
