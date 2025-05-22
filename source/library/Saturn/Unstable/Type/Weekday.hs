{-# LANGUAGE FlexibleContexts #-}

module Saturn.Unstable.Type.Weekday where

import qualified Data.Coerce as Coerce
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Type.Field as Field
import qualified Text.Parsec as Parsec

newtype Weekday
  = Weekday Field.Field
  deriving (Eq, Show)

bounds :: (Word.Word8, Word.Word8)
bounds = (0, 6)

fromField :: Field.Field -> Maybe Weekday
fromField field =
  if Field.isValid bounds field then Just $ Weekday field else Nothing

toField :: Weekday -> Field.Field
toField = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Weekday
parsec = do
  field <- Field.parsec
  maybe (fail "invalid Weekday") pure $ fromField field

toBuilder :: Weekday -> Builder.Builder
toBuilder = Field.toBuilder . toField

expand :: Weekday -> Set.Set Word.Word8
expand = Field.expand bounds . toField

isMatch :: Word.Word8 -> Weekday -> Bool
isMatch word8 = Set.member word8 . expand
