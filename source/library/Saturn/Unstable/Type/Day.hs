module Saturn.Unstable.Type.Day where

import qualified Data.Coerce as Coerce
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Type.Field as Field
import qualified Text.Parsec as Parsec

newtype Day
  = Day Field.Field
  deriving (Eq, Show)

bounds :: (Word.Word8, Word.Word8)
bounds = (1, 31)

fromField :: Field.Field -> Maybe Day
fromField field =
  if Field.isValid bounds field then Just $ Day field else Nothing

toField :: Day -> Field.Field
toField = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Day
parsec = do
  field <- Field.parsec
  maybe (fail "invalid Day") pure $ fromField field

toBuilder :: Day -> Builder.Builder
toBuilder = Field.toBuilder . toField

expand :: Day -> Set.Set Word.Word8
expand = Field.expand bounds . toField

isMatch :: Word.Word8 -> Day -> Bool
isMatch word8 = Set.member word8 . expand
