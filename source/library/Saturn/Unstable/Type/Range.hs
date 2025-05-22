{-# LANGUAGE FlexibleContexts #-}

module Saturn.Unstable.Type.Range where

import qualified Control.Monad as Monad
import qualified Data.Coerce as Coerce
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Extra.Tuple as Tuple
import qualified Saturn.Unstable.Type.Number as Number
import qualified Text.Parsec as Parsec

newtype Range
  = Range (Number.Number, Number.Number)
  deriving (Eq, Show)

fromTuple :: (Number.Number, Number.Number) -> Maybe Range
fromTuple (lo, hi) =
  if Number.toWord8 lo > Number.toWord8 hi
    then Nothing
    else Just $ Range (lo, hi)

toTuple :: Range -> (Number.Number, Number.Number)
toTuple = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Range
parsec = do
  lo <- Number.parsec
  Monad.void $ Parsec.char '-'
  hi <- Number.parsec
  maybe (fail "invalid Range") pure $ fromTuple (lo, hi)

toBuilder :: Range -> Builder.Builder
toBuilder range =
  let (lo, hi) = toTuple range
   in Number.toBuilder lo <> Builder.singleton '-' <> Number.toBuilder hi

isValid :: (Word.Word8, Word.Word8) -> Range -> Bool
isValid tuple = uncurry (&&) . Tuple.mapBoth (Number.isValid tuple) . toTuple

expand :: Range -> Set.Set Word.Word8
expand =
  Set.fromList
    . Tuple.toSequence
    . Tuple.mapBoth Number.toWord8
    . toTuple
