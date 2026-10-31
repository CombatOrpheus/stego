-- | Hand-rolled ChaCha20 stream cipher (RFC 8439, 256-bit key, 32-bit counter).
--
-- The stego tool uses this in place of the former DES implementation:
-- every "block" is a single character encoded as a 'Word64', so we treat each
-- block as its own message and derive a per-block nonce from the key and the
-- block index.  Encryption and decryption are the same operation (XOR with
-- the keystream), exactly like the old ECB-style DES usage, but built on a
-- stream cipher instead of a hand-rolled Feistel network.
module ChaCha20
  ( encryptMessage
  , decryptMessage
  , chacha20Block
  , quarterRound
  ) where

import Data.Bits
import Data.Word
import WordOperations (integerToWord64)

-- ChaCha20 constants: the expansion of "expand 32-byte k", little-endian words.
sigma0, sigma1, sigma2, sigma3 :: Word32
sigma0 = 0x61707865
sigma1 = 0x3320646e
sigma2 = 0x79622d32
sigma3 = 0x6b206574

rotl32 :: Int -> Word32 -> Word32
rotl32 n x = (x `shiftL` n) .|. (x `shiftR` (32 - n))

-- | The ChaCha quarter round (RFC 8439, section 2.1.1):
-- a += b; d ^= a; d <<<< 16; c += d; b ^= c; b <<<< 12;
-- a += b; d ^= a; d <<<<  8; c += d; b ^= c; b <<<<  7;
quarterRound :: Word32 -> Word32 -> Word32 -> Word32 -> (Word32, Word32, Word32, Word32)
quarterRound a0 b0 c0 d0 = (a3, b3, c3, d3)
  where
    a1 = a0 + b0
    d1 = rotl32 16 (d0 `xor` a1)
    c1 = c0 + d1
    b1 = rotl32 12 (b0 `xor` c1)
    a2 = a1 + b1
    d2 = rotl32 8 (d1 `xor` a2)
    c2 = c1 + d2
    b2 = rotl32 7 (b1 `xor` c2)
    a3 = a2 + b2
    d3 = rotl32 16 (d2 `xor` a3)
    c3 = c2 + d3
    b3 = rotl32 12 (b2 `xor` c3)

type State = [Word32]

columnRound :: State -> State
columnRound s = replaceAll [ qr 0 4 8 12, qr 1 5 9 13, qr 2 6 10 14, qr 3 7 11 15 ] s
  where qr i j k l = (i, quarterRound (s !! i) (s !! j) (s !! k) (s !! l))

diagonalRound :: State -> State
diagonalRound s = replaceAll [ qr 0 5 10 15, qr 1 6 11 12, qr 2 7 8 13, qr 3 4 9 14 ] s
  where qr i j k l = (i, quarterRound (s !! i) (s !! j) (s !! k) (s !! l))

replaceAll :: [(Int, (Word32, Word32, Word32, Word32))] -> State -> State
replaceAll [] s = s
replaceAll edits s = [ replaceAt j sJ | (j, sJ) <- zip [0 ..] s ]
  where
    -- (base index, new state) pairs produced by each quarter round in parallel
    applied = [ (i, put (i + 12) d (put (i + 8) c (put (i + 4) b (put i a s))))
              | (i, (a, b, c, d)) <- edits ]
    replaceAt j sJ = case filter ((== j) . fst) applied of
                       ((_, st):_) -> st !! j
                       []          -> sJ
    put i v xs = [ if k == i then v else x | (k, x) <- zip [0 ..] xs ]

-- | One ChaCha20 block (64 bytes) of keystream for the given 256-bit key
-- (eight 'Word32' words), 96-bit nonce (three 'Word32' words) and 32-bit
-- block counter, exactly as specified in RFC 8439.
chacha20Block :: [Word32] -> [Word32] -> Word32 -> [Word32]
chacha20Block keyWords nonceWords counter = zipWith (+) stateOut initialState
  where
    initialState =
      [ sigma0, sigma1, sigma2, sigma3 ]
        ++ [ keyWords !! 0, keyWords !! 1, keyWords !! 2, keyWords !! 3
           , keyWords !! 4, keyWords !! 5, keyWords !! 6, keyWords !! 7
           , counter
           , nonceWords !! 0, nonceWords !! 1, nonceWords !! 2 ]
    stateOut = foldr ($) initialState (replicate 10 (diagonalRound . columnRound))

-- | Turn a decimal digit string into a 256-bit key: eight 'Word32' words,
-- derived deterministically from the digits with an avalanche step.
stringToKey :: [Char] -> [Word32]
stringToKey digits = map mix (zip [0 ..] rawWords)
  where
    digitValues = [ fromIntegral (ord' c) | c <- digits, isDigit' c ]
    base = if null digitValues then [1] else digitValues
    padded = take 32 (base ++ [ v * 7 + 13 + fromIntegral i | (i, v) <- zip [0 ..] (cycle base) ])
    rawWords = map wordFromChunk (chunk4 padded)
    mix (i, w) = w * 0x9e3779b9 + fromIntegral i * 0x85ebca6b + 0xc2b2ae35
    wordFromChunk ds = foldl (\acc d -> acc * 10 + d) 0 ds
    chunk4 [] = []
    chunk4 xs = take 4 xs : chunk4 (drop 4 xs)
    ord' c = fromEnum c - fromEnum '0'
    isDigit' c = c >= '0' && c <= '9'

mix32 :: Word32 -> Word32 -> Word32
mix32 a b = rotl32 17 ((a * 0x9e3779b9) `xor` (b + 0x243f6a88))

deriveNonce :: [Word32] -> Int -> [Word32]
deriveNonce ks index = [ n0, n1, n2 ]
  where
    seed = fromIntegral index :: Word32
    n0 = foldl mix32 seed ks
    n1 = foldl mix32 (n0 `xor` 0x5bf03635) (reverse ks)
    n2 = foldl mix32 (n1 `xor` 0x1f83d9ab) ks

keystreamWord :: [Word32] -> Int -> Word64 -> Word64
keystreamWord ks index block =
  let kw = chacha20Block ks (deriveNonce ks index) (fromIntegral index)
      w1 = (fromIntegral (kw !! 0) :: Word64) `shiftL` 32 .|. fromIntegral (kw !! 1)
      w2 = (fromIntegral (kw !! 2) :: Word64) `shiftL` 32 .|. fromIntegral (kw !! 3)
  in block `xor` w1 `xor` w2

cryptBlocks :: [Char] -> [Word64] -> [Word64]
cryptBlocks key blocks = zipWith (keystreamWord ks) [0 ..] blocks
  where
    ks = stringToKey key

-- | Encrypt a message (one 'Word64' per character) under a decimal key.
encryptMessage :: Integer -> [Char] -> [Word64]
encryptMessage key = cryptBlocks (show key) . map toWord
  where toWord = fromIntegral . fromEnum

-- | Decrypt an encrypted message back to the original 'Word64' blocks.
decryptMessage :: Integer -> [Integer] -> [Word64]
decryptMessage key = cryptBlocks (show key) . integerToWord64
