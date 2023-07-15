module Saturn.Unstable.Type.Element where

import qualified Data.Coerce as Coerce
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Extra.Parsec as Parsec
import qualified Saturn.Unstable.Type.Number as Number
import qualified Saturn.Unstable.Type.Range as Range
import qualified Text.Parsec as Parsec

newtype Element
  = Element (Either Range.Range Number.Number)
  deriving (Eq, Show)

fromEither :: Either Range.Range Number.Number -> Element
fromEither = Coerce.coerce

toEither :: Element -> Either Range.Range Number.Number
toEither = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Element
parsec = fromEither <$> Parsec.either (Parsec.try Range.parsec) Number.parsec

toBuilder :: Element -> Builder.Builder
toBuilder = either Range.toBuilder Number.toBuilder . toEither

isValid :: (Word.Word8, Word.Word8) -> Element -> Bool
isValid tuple = either (Range.isValid tuple) (Number.isValid tuple) . toEither

expand :: Element -> Set.Set Word.Word8
expand = either Range.expand (Set.singleton . Number.toWord8) . toEither
