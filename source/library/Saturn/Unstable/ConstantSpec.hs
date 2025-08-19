module Saturn.Unstable.ConstantSpec where

import qualified Heck
import qualified Saturn.Unstable.Constant as Constant
import qualified Saturn.Unstable.Render as Render

spec :: (Applicative m, Monad n) => Heck.Test m n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Constant" $ do
  Heck.describe t "everyMinute" $ do
    Heck.it t "renders correctly" $ do
      Heck.assertEq t "* * * * *" (Render.toString Constant.everyMinute)

  Heck.describe t "hourly" $ do
    Heck.it t "renders correctly" $ do
      Heck.assertEq t "0 * * * *" (Render.toString Constant.hourly)

  Heck.describe t "daily" $ do
    Heck.it t "renders correctly" $ do
      Heck.assertEq t "0 0 * * *" (Render.toString Constant.daily)

  Heck.describe t "weekly" $ do
    Heck.it t "renders correctly" $ do
      Heck.assertEq t "0 0 * * 0" (Render.toString Constant.weekly)

  Heck.describe t "monthly" $ do
    Heck.it t "renders correctly" $ do
      Heck.assertEq t "0 0 1 * *" (Render.toString Constant.monthly)

  Heck.describe t "yearly" $ do
    Heck.it t "renders correctly" $ do
      Heck.assertEq t "0 0 1 1 *" (Render.toString Constant.yearly)
