module Saturn.Unstable.RenderSpec where

import qualified Heck
import qualified Saturn.Unstable.Render as Render
import qualified Saturn.Unstable.Type.ScheduleSpec as ScheduleSpec

spec :: (MonadFail m, Monad n) => Heck.Test m n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Render" $ do
  Heck.describe t "toString" $ do
    Heck.it t "works with wildcards" $ do
      schedule <- ScheduleSpec.new [] [] [] [] []
      Heck.assertEq t "* * * * *" (Render.toString schedule)

    Heck.it t "works with numbers" $ do
      schedule <- ScheduleSpec.new [[4]] [[3]] [[2]] [[1]] [[0]]
      Heck.assertEq t "4 3 2 1 0" (Render.toString schedule)

    Heck.it t "works with ranges" $ do
      schedule <- ScheduleSpec.new [[8, 9]] [[6, 7]] [[4, 5]] [[2, 3]] [[0, 1]]
      Heck.assertEq t "8-9 6-7 4-5 2-3 0-1" (Render.toString schedule)

    Heck.it t "works with choices" $ do
      schedule <- ScheduleSpec.new [[8], [9]] [[6], [7]] [[4], [5]] [[2], [3]] [[0], [1]]
      Heck.assertEq t "8,9 6,7 4,5 2,3 0,1" (Render.toString schedule)
