module SaturnSpec where

import qualified Saturn.Unstable.Parse as Parse
import qualified Saturn.Unstable.Render as Render
import qualified Saturn.Unstable.Type.ScheduleSpec as ScheduleSpec
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Saturn" $ do
  Hspec.describe "round trips" $ do
    Hspec.it "through string"
      . QuickCheck.forAllShrink ScheduleSpec.arbitrary ScheduleSpec.shrink
      $ \schedule ->
        Parse.fromString (Render.toString schedule) `Hspec.shouldBe` Right schedule

    Hspec.it "through strict text"
      . QuickCheck.forAllShrink ScheduleSpec.arbitrary ScheduleSpec.shrink
      $ \schedule ->
        Parse.fromText (Render.toText schedule) `Hspec.shouldBe` Right schedule

    Hspec.it "through lazy text"
      . QuickCheck.forAllShrink ScheduleSpec.arbitrary ScheduleSpec.shrink
      $ \schedule ->
        Parse.fromLazyText (Render.toLazyText schedule) `Hspec.shouldBe` Right schedule
