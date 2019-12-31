module System.Nix.Base32 (encode) where

-- Copied from hnix-store-core until there's a new release

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Protolude

-- | Encode a 'BS.ByteString' in Nix's base32 encoding
encode :: BS.ByteString -> T.Text
encode c = T.pack $ map char32 [nChar - 1, nChar - 2 .. 0]
  where
    digits32 = V.fromList "0123456789abcdfghijklmnpqrsvwxyz"
    -- Each base32 character gives us 5 bits of information, while
    -- each byte gives is 8. Because 'div' rounds down, we need to add
    -- one extra character to the result, and because of that extra 1
    -- we need to subtract one from the number of bits in the
    -- bytestring to cover for the case where the number of bits is
    -- already a factor of 5. Thus, the + 1 outside of the 'div' and
    -- the - 1 inside of it.
    nChar = fromIntegral $ ((BS.length c * 8 - 1) `div` 5) + 1
    byte = BS.index c . fromIntegral
    -- May need to switch to a more efficient calculation at some
    -- point.
    bAsInteger :: Integer
    bAsInteger =
      sum
        [ fromIntegral (byte j) * (256 ^ j)
          | j <- [0 .. BS.length c - 1]
        ]
    char32 :: Integer -> Char
    char32 i = digits32 V.! digitInd
      where
        digitInd =
          fromIntegral $
            bAsInteger
              `div` (32 ^ i)
              `mod` 32
