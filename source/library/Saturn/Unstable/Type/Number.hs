{-# LANGUAGE FlexibleContexts #-}

module Saturn.Unstable.Type.Number where

import qualified Data.Bits as Bits
import qualified Data.Coerce as Coerce
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Extra.Ord as Ord
import qualified Text.Parsec as Parsec
import qualified Text.Read as Read

newtype Number
  = Number Word.Word8
  deriving (Eq, Show)

fromWord8 :: Word.Word8 -> Number
fromWord8 = Coerce.coerce

toWord8 :: Number -> Word.Word8
toWord8 = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Number
parsec = do
  string <- Parsec.many1 Parsec.digit
  integer <- maybe (fail "invalidNumber") pure $ Read.readMaybe string
  maybe (fail "invalidNumber") (pure . fromWord8) $
    Bits.toIntegralSized (integer :: Integer)

toBuilder :: Number -> Builder.Builder
toBuilder = Builder.decimal . toWord8

isValid :: (Word.Word8, Word.Word8) -> Number -> Bool
isValid tuple = Ord.within tuple . toWord8
