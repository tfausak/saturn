module Saturn.Unstable.ConstantSpec where

import qualified Saturn.Unstable.Constant as Constant
import qualified Saturn.Unstable.Render as Render
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Constant" $ do
  Hspec.describe "everyMinute" $ do
    Hspec.it "renders correctly" $ do
      Render.toString Constant.everyMinute `Hspec.shouldBe` "* * * * *"

  Hspec.describe "hourly" $ do
    Hspec.it "renders correctly" $ do
      Render.toString Constant.hourly `Hspec.shouldBe` "0 * * * *"

  Hspec.describe "daily" $ do
    Hspec.it "renders correctly" $ do
      Render.toString Constant.daily `Hspec.shouldBe` "0 0 * * *"

  Hspec.describe "weekly" $ do
    Hspec.it "renders correctly" $ do
      Render.toString Constant.weekly `Hspec.shouldBe` "0 0 * * 0"

  Hspec.describe "monthly" $ do
    Hspec.it "renders correctly" $ do
      Render.toString Constant.monthly `Hspec.shouldBe` "0 0 1 * *"

  Hspec.describe "yearly" $ do
    Hspec.it "renders correctly" $ do
      Render.toString Constant.yearly `Hspec.shouldBe` "0 0 1 1 *"
