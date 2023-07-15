module Saturn.Unstable.Type.Field where

import qualified Data.Coerce as Coerce
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Extra.Parsec as Parsec
import qualified Saturn.Unstable.Extra.Tuple as Tuple
import qualified Saturn.Unstable.Type.Element as Element
import qualified Saturn.Unstable.Type.Wildcard as Wildcard
import qualified Text.Parsec as Parsec

newtype Field
  = Field (Either Wildcard.Wildcard (NonEmpty.NonEmpty Element.Element))
  deriving (Eq, Show)

fromEither ::
  Either Wildcard.Wildcard (NonEmpty.NonEmpty Element.Element) -> Field
fromEither = Coerce.coerce

toEither ::
  Field -> Either Wildcard.Wildcard (NonEmpty.NonEmpty Element.Element)
toEither = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Field
parsec =
  fromEither
    <$> Parsec.either
      Wildcard.parsec
      (Parsec.sepByNE Element.parsec $ Parsec.char ',')

toBuilder :: Field -> Builder.Builder
toBuilder =
  either
    Wildcard.toBuilder
    ( Foldable.fold
        . NonEmpty.intersperse (Builder.singleton ',')
        . fmap Element.toBuilder
    )
    . toEither

isValid :: (Word.Word8, Word.Word8) -> Field -> Bool
isValid tuple = either (const True) (all $ Element.isValid tuple) . toEither

expand :: (Word.Word8, Word.Word8) -> Field -> Set.Set Word.Word8
expand tuple =
  either
    (const . Set.fromList $ Tuple.toSequence tuple)
    (Set.unions . fmap Element.expand)
    . toEither

isWildcard :: Field -> Bool
isWildcard = Either.isLeft . toEither
