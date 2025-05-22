module Saturn.Unstable.RenderSpec where

import qualified Saturn.Unstable.Render as Render
import qualified Saturn.Unstable.Type.ScheduleSpec as ScheduleSpec
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Render" $ do
  Hspec.describe "toString" $ do
    Hspec.it "works with wildcards" $ do
      schedule <- ScheduleSpec.new [] [] [] [] []
      Render.toString schedule `Hspec.shouldBe` "* * * * *"

    Hspec.it "works with numbers" $ do
      schedule <- ScheduleSpec.new [[4]] [[3]] [[2]] [[1]] [[0]]
      Render.toString schedule `Hspec.shouldBe` "4 3 2 1 0"

    Hspec.it "works with ranges" $ do
      schedule <- ScheduleSpec.new [[8, 9]] [[6, 7]] [[4, 5]] [[2, 3]] [[0, 1]]
      Render.toString schedule `Hspec.shouldBe` "8-9 6-7 4-5 2-3 0-1"

    Hspec.it "works with choices" $ do
      schedule <- ScheduleSpec.new [[8], [9]] [[6], [7]] [[4], [5]] [[2], [3]] [[0], [1]]
      Render.toString schedule `Hspec.shouldBe` "8,9 6,7 4,5 2,3 0,1"
