-- | Command-line entry point for the @chacha20-stego@ binary.
--
-- Usage:
--
-- > chacha20-stego [I|E] [<image-path> [<key> [<message>]]]
-- >
-- >   I  insert : embed an encrypted message into a PPM (P3) image,
-- >              writing the result to @<image-path>1@
-- >   E  extract: recover and decrypt the hidden message, printing it and
-- >              writing it to @<image-path>.txt@
-- >
-- > Options:
-- >   -o <file>   write output to <file> instead of the default path
-- >   -h, --help  show this usage message
--
-- Any argument left out is prompted for interactively, so running the
-- binary with no arguments behaves like the original interactive tool.
module Main (main) where

import Control.Exception (SomeException, try)
import Data.Char (toUpper)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)

import IoOperations
  ( embedMessage
  , getImage
  , getKey
  , getImagePath
  , getMessage
  , getStegnoKey
  , keyToNum
  , recoverMessage
  )

-- | One parsed command line.  'Nothing' fields are filled in by prompting
-- at run time.
data Config = Config
  { cfgOperation :: Maybe Char   -- ^ @'Just \'I'@ or @'Just \'E'@
  , cfgImage     :: Maybe FilePath
  , cfgKey       :: Maybe String
  , cfgMessage   :: Maybe String
  , cfgOutput    :: Maybe FilePath
  , cfgHelp      :: Bool
  }

defaultConfig :: Config
defaultConfig = Config Nothing Nothing Nothing Nothing Nothing False

main :: IO ()
main = do
  argv <- getArgs
  case parseArgs defaultConfig argv of
    Left err -> do
      putStrLn $ "Error: " ++ err
      usage
      exitFailure
    Right cfg
      | cfgHelp cfg -> usage >> exitSuccess
      | otherwise   -> run cfg

-- | Argument parser.  Bare positional arguments fill the slots
-- operation → image → key → message, in order; flags can set any slot
-- explicitly and may appear anywhere.
parseArgs :: Config -> [String] -> Either String Config
parseArgs cfg [] = Right cfg
parseArgs cfg (a:rest) = case a of
  "-h"        -> parseArgs cfg { cfgHelp = True } rest
  "--help"    -> parseArgs cfg { cfgHelp = True } rest
  "-i"        -> withArg rest "image path" $ \v more ->
                   parseArgs cfg { cfgImage = Just v } more
  "--image"   -> withArg rest "image path" $ \v more ->
                   parseArgs cfg { cfgImage = Just v } more
  "-k"        -> withArg rest "key" $ \v more ->
                   parseArgs cfg { cfgKey = Just v } more
  "--key"     -> withArg rest "key" $ \v more ->
                   parseArgs cfg { cfgKey = Just v } more
  "-m"        -> withArg rest "message" $ \v more ->
                   parseArgs cfg { cfgMessage = Just v } more
  "--message" -> withArg rest "message" $ \v more ->
                   parseArgs cfg { cfgMessage = Just v } more
  "-o"        -> withArg rest "output path" $ \v more ->
                   parseArgs cfg { cfgOutput = Just v } more
  "--output"  -> withArg rest "output path" $ \v more ->
                   parseArgs cfg { cfgOutput = Just v } more
  ('-':_)     -> Left $ "unknown option " ++ show a
  _           -> parseArgs (fillPositional cfg a) rest

-- | @withArg rest what f@ consumes the head of @rest@ as the value of an
-- option, applying @f@ to it and the remaining arguments.
withArg :: [String] -> String
        -> (String -> [String] -> Either String Config)
        -> Either String Config
withArg (v:more) _   f = f v more
withArg []       what _ = Left $ "option requires an argument: " ++ what

-- | A one-character operation token claims the operation slot first;
-- anything else fills the next empty positional slot.
fillPositional :: Config -> String -> Config
fillPositional c s
  | isOp s && not (isSet (cfgOperation c)) = c { cfgOperation = Just (opChar s) }
  | not (isSet (cfgImage c))               = c { cfgImage = Just s }
  | not (isSet (cfgKey c))                 = c { cfgKey = Just s }
  | otherwise                              = c { cfgMessage = Just s }
  where
    isSet :: Maybe a -> Bool
    isSet = maybe False (const True)
    isOp t = length t == 1 && (opChar t == 'I' || opChar t == 'E')
    opChar t = toUpper (head t)

-- | Dispatch on the (possibly prompted-for) operation and execute it.
run :: Config -> IO ()
run cfg = do
  op <- case cfgOperation cfg of
    Just o  -> return o
    Nothing -> do
      putStrLn "Desired Operation: [I]nsert / [E]xtract message:"
      line <- getLine
      return (if null line then 'X' else toUpper (head line))
  case op of
    'I' -> insert cfg
    'E' -> extract cfg
    _   -> putStrLn "Wrong Operation: expected I (insert) or E (extract)."

-- | Insert workflow: use CLI values when present, prompt for the rest.
insert :: Config -> IO ()
insert cfg = do
  path <- orPrompt (cfgImage cfg) getImagePath
  message <- orPrompt (cfgMessage cfg) getMessage
  keyStr <- orPrompt (cfgKey cfg) getKey
  image <- readImage path
  case embedMessage (keyToNum keyStr) message image of
    Nothing ->
      putStrLn "Image is too small: choose a bigger image or a smaller message."
    Just stegoImage -> do
      let out = maybe (path ++ "1") id (cfgOutput cfg)
      getStegnoKey out stegoImage
      putStrLn ("Stego image written to " ++ out)

-- | Extract workflow: use CLI values when present, prompt for the rest.
extract :: Config -> IO ()
extract cfg = do
  path <- orPrompt (cfgImage cfg) getImagePath
  keyStr <- orPrompt (cfgKey cfg) getKey
  image <- readImage path
  let finalMessage = recoverMessage (keyToNum keyStr) image
  putStrLn finalMessage
  case cfgOutput cfg of
    Just out -> writeFile out finalMessage
    Nothing  -> writeFile (path ++ ".txt") finalMessage

-- | Use the supplied value, or prompt the user when it is missing.
orPrompt :: Maybe a -> IO a -> IO a
orPrompt (Just v) _  = return v
orPrompt Nothing  io = io

-- | Read an image file, turning IO failures (missing file, bad permissions)
-- into a clean error message instead of a raw Haskell exception.
readImage :: FilePath -> IO String
readImage path = do
  result <- try (getImage path) :: IO (Either SomeException String)
  case result of
    Right image -> return image
    Left  err   -> do
      putStrLn ("Could not read image " ++ show path ++ ": " ++ show err)
      exitFailure

usage :: IO ()
usage = do
  name <- getProgName
  putStrLn $ unlines
    [ "Usage: " ++ name ++ " [I|E] [<image> [<key> [<message>]]] [-o <output>]"
    , ""
    , "  I            insert a message into a PPM (P3) image"
    , "  E            extract a message from a stego PPM image"
    , "  <image>      path to the input image"
    , "  <key>        numeric encryption/decryption key"
    , "  <message>    plaintext to embed (insert only)"
    , "  -o <output>  output file (default: <image>1 / <image>.txt)"
    , "  -h, --help   show this message"
    , ""
    , "Missing arguments are requested interactively; with no arguments"
    , "at all the tool runs fully interactively."
    ]
