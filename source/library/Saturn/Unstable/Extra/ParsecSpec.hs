{-# LANGUAGE FlexibleContexts #-}

module Saturn.Unstable.Extra.ParsecSpec where

import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Heck
import qualified Saturn.Unstable.Extra.Parsec as Parsec
import qualified Text.Parsec as Parsec

spec :: (Applicative m, Monad n) => Heck.Test m n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Extra.Parsec" $ do
  Heck.describe t "either" $ do
    let parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m (Either Char Char)
        parsec = Parsec.either (Parsec.char 'a') (Parsec.char 'b')

    Heck.it t "succeeds with left" $ do
      Heck.assertEq t (Right (Left 'a')) (Parsec.parse parsec "" "a")

    Heck.it t "succeeds with right" $ do
      Heck.assertEq t (Right (Right 'b')) (Parsec.parse parsec "" "b")

    Heck.it t "fails with neither" $ do
      Heck.assertEq t True (Either.isLeft (Parsec.parse parsec "" "c"))

  Heck.describe t "sepByNE" $ do
    let parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m (NonEmpty.NonEmpty Char)
        parsec = Parsec.sepByNE (Parsec.char 'a') (Parsec.char ' ')

    Heck.it t "succeeds with one" $ do
      Heck.assertEq t (Right ('a' NonEmpty.:| [])) (Parsec.parse parsec "" "a")

    Heck.it t "succeeds with many" $ do
      Heck.assertEq t (Right ('a' NonEmpty.:| "a")) (Parsec.parse parsec "" "a a")

    Heck.it t "fails with none" $ do
      Heck.assertEq t True (Either.isLeft (Parsec.parse parsec "" ""))
