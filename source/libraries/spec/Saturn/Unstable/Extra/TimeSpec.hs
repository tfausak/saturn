module Saturn.Unstable.Extra.TimeSpec where

import qualified Data.Time as Time
import qualified Saturn.Unstable.Extra.Time as Time
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Extra.Time" $ do
  Hspec.describe "dayOfWeekToWord8" $ do
    Hspec.it "works with Sunday" $ do
      Time.dayOfWeekToWord8 Time.Sunday `Hspec.shouldBe` 0

    Hspec.it "works with Monday" $ do
      Time.dayOfWeekToWord8 Time.Monday `Hspec.shouldBe` 1

    Hspec.it "works with Tuesday" $ do
      Time.dayOfWeekToWord8 Time.Tuesday `Hspec.shouldBe` 2

    Hspec.it "works with Wednesday" $ do
      Time.dayOfWeekToWord8 Time.Wednesday `Hspec.shouldBe` 3

    Hspec.it "works with Thursday" $ do
      Time.dayOfWeekToWord8 Time.Thursday `Hspec.shouldBe` 4

    Hspec.it "works with Friday" $ do
      Time.dayOfWeekToWord8 Time.Friday `Hspec.shouldBe` 5

    Hspec.it "works with Saturday" $ do
      Time.dayOfWeekToWord8 Time.Saturday `Hspec.shouldBe` 6
