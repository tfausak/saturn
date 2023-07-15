module Saturn.Unstable.Type.Month where

import qualified Data.Coerce as Coerce
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Type.Field as Field
import qualified Text.Parsec as Parsec

newtype Month
  = Month Field.Field
  deriving (Eq, Show)

bounds :: (Word.Word8, Word.Word8)
bounds = (1, 12)

fromField :: Field.Field -> Maybe Month
fromField field =
  if Field.isValid bounds field then Just $ Month field else Nothing

toField :: Month -> Field.Field
toField = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Month
parsec = do
  field <- Field.parsec
  maybe (fail "invalid Month") pure $ fromField field

toBuilder :: Month -> Builder.Builder
toBuilder = Field.toBuilder . toField

expand :: Month -> Set.Set Word.Word8
expand = Field.expand bounds . toField

isMatch :: Word.Word8 -> Month -> Bool
isMatch word8 = Set.member word8 . expand
