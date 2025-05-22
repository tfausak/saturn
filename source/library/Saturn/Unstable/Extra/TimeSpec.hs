module Saturn.Unstable.Extra.TimeSpec where

import qualified Data.Time as Time
import qualified Heck
import qualified Saturn.Unstable.Extra.Time as Time

spec :: (Applicative m, Monad n) => Heck.Test m n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Extra.Time" $ do
  Heck.describe t "dayOfWeekToWord8" $ do
    Heck.it t "works with Sunday" $ do
      Heck.assertEq t 0 (Time.dayOfWeekToWord8 Time.Sunday)

    Heck.it t "works with Monday" $ do
      Heck.assertEq t 1 (Time.dayOfWeekToWord8 Time.Monday)

    Heck.it t "works with Tuesday" $ do
      Heck.assertEq t 2 (Time.dayOfWeekToWord8 Time.Tuesday)

    Heck.it t "works with Wednesday" $ do
      Heck.assertEq t 3 (Time.dayOfWeekToWord8 Time.Wednesday)

    Heck.it t "works with Thursday" $ do
      Heck.assertEq t 4 (Time.dayOfWeekToWord8 Time.Thursday)

    Heck.it t "works with Friday" $ do
      Heck.assertEq t 5 (Time.dayOfWeekToWord8 Time.Friday)

    Heck.it t "works with Saturday" $ do
      Heck.assertEq t 6 (Time.dayOfWeekToWord8 Time.Saturday)
