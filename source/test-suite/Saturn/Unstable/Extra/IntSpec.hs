module Saturn.Unstable.Extra.IntSpec where

import qualified Saturn.Unstable.Extra.Int as Int
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Extra.Int" $ do
  Hspec.describe "fromWord8" $ do
    Hspec.it "works" $ do
      Int.fromWord8 0 `Hspec.shouldBe` 0

  Hspec.describe "toWord8" $ do
    Hspec.it "succeeds with minimum" $ do
      Int.toWord8 0 `Hspec.shouldBe` Just 0

    Hspec.it "succeeds with maximum" $ do
      Int.toWord8 255 `Hspec.shouldBe` Just 255

    Hspec.it "fails below minimum" $ do
      Int.toWord8 (-1) `Hspec.shouldBe` Nothing

    Hspec.it "fails above maximum" $ do
      Int.toWord8 256 `Hspec.shouldBe` Nothing
