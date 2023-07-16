module Saturn.Unstable.Extra.Int where

import qualified Data.Bits as Bits
import qualified Data.Word as Word

fromWord8 :: Word.Word8 -> Int
fromWord8 = fromIntegral

toWord8 :: Int -> Maybe Word.Word8
toWord8 = Bits.toIntegralSized
