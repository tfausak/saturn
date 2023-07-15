module Saturn.Unstable.Type.Hour where

import qualified Data.Coerce as Coerce
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Type.Field as Field
import qualified Text.Parsec as Parsec

newtype Hour
  = Hour Field.Field
  deriving (Eq, Show)

bounds :: (Word.Word8, Word.Word8)
bounds = (0, 23)

fromField :: Field.Field -> Maybe Hour
fromField field =
  if Field.isValid bounds field then Just $ Hour field else Nothing

toField :: Hour -> Field.Field
toField = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Hour
parsec = do
  field <- Field.parsec
  maybe (fail "invalid Hour") pure $ fromField field

toBuilder :: Hour -> Builder.Builder
toBuilder = Field.toBuilder . toField

expand :: Hour -> Set.Set Word.Word8
expand = Field.expand bounds . toField

isMatch :: Word.Word8 -> Hour -> Bool
isMatch word8 = Set.member word8 . expand
