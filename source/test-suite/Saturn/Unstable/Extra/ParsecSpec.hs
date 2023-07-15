module Saturn.Unstable.Extra.ParsecSpec where

import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Saturn.Unstable.Extra.Parsec as Parsec
import qualified Test.Hspec as Hspec
import qualified Text.Parsec as Parsec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Extra.Parsec" $ do
  Hspec.describe "either" $ do
    let parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m (Either Char Char)
        parsec = Parsec.either (Parsec.char 'a') (Parsec.char 'b')

    Hspec.it "succeeds with left" $ do
      Parsec.parse parsec "" "a" `Hspec.shouldBe` Right (Left 'a')

    Hspec.it "succeeds with right" $ do
      Parsec.parse parsec "" "b" `Hspec.shouldBe` Right (Right 'b')

    Hspec.it "fails with neither" $ do
      Parsec.parse parsec "" "c" `Hspec.shouldSatisfy` Either.isLeft

  Hspec.describe "sepByNE" $ do
    let parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m (NonEmpty.NonEmpty Char)
        parsec = Parsec.sepByNE (Parsec.char 'a') (Parsec.char ' ')

    Hspec.it "succeeds with one" $ do
      Parsec.parse parsec "" "a" `Hspec.shouldBe` Right ('a' NonEmpty.:| [])

    Hspec.it "succeeds with many" $ do
      Parsec.parse parsec "" "a a" `Hspec.shouldBe` Right ('a' NonEmpty.:| "a")

    Hspec.it "fails with none" $ do
      Parsec.parse parsec "" "" `Hspec.shouldSatisfy` Either.isLeft
