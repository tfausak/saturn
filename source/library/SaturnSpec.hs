module SaturnSpec where

import qualified Heck
import qualified Saturn.Unstable.Parse as Parse
import qualified Saturn.Unstable.Render as Render
import qualified Saturn.Unstable.Type.ScheduleSpec as ScheduleSpec
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: (Monad n) => Heck.Test IO n -> n ()
spec t = Heck.describe t "Saturn" $ do
  Heck.describe t "round trips" $ do
    Heck.it t "through string"
      . QuickCheck.quickCheck
      . QuickCheck.forAllShrink ScheduleSpec.arbitrary ScheduleSpec.shrink
      $ \schedule ->
        Parse.fromString (Render.toString schedule) `Hspec.shouldBe` Right schedule

    Heck.it t "through strict text"
      . QuickCheck.quickCheck
      . QuickCheck.forAllShrink ScheduleSpec.arbitrary ScheduleSpec.shrink
      $ \schedule ->
        Parse.fromText (Render.toText schedule) `Hspec.shouldBe` Right schedule

    Heck.it t "through lazy text"
      . QuickCheck.quickCheck
      . QuickCheck.forAllShrink ScheduleSpec.arbitrary ScheduleSpec.shrink
      $ \schedule ->
        Parse.fromLazyText (Render.toLazyText schedule) `Hspec.shouldBe` Right schedule
