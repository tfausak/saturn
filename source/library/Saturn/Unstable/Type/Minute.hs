module Saturn.Unstable.Type.Minute where

import qualified Data.Coerce as Coerce
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Type.Field as Field
import qualified Text.Parsec as Parsec

newtype Minute
  = Minute Field.Field
  deriving (Eq, Show)

bounds :: (Word.Word8, Word.Word8)
bounds = (0, 59)

fromField :: Field.Field -> Maybe Minute
fromField field =
  if Field.isValid bounds field then Just $ Minute field else Nothing

toField :: Minute -> Field.Field
toField = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Minute
parsec = do
  field <- Field.parsec
  maybe (fail "invalid Minute") pure $ fromField field

toBuilder :: Minute -> Builder.Builder
toBuilder = Field.toBuilder . toField

expand :: Minute -> Set.Set Word.Word8
expand = Field.expand bounds . toField

isMatch :: Word.Word8 -> Minute -> Bool
isMatch word8 = Set.member word8 . expand
