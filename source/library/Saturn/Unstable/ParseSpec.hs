module Saturn.Unstable.ParseSpec where

import qualified Data.Either as Either
import qualified Heck
import qualified Saturn.Unstable.Parse as Parse
import qualified Saturn.Unstable.Type.ScheduleSpec as ScheduleSpec

spec :: (MonadFail m, Monad n) => Heck.Test m n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Parse" $ do
  Heck.describe t "fromString" $ do
    Heck.it t "accepts wildcards" $ do
      schedule <- ScheduleSpec.new [] [] [] [] []
      Heck.assertEq t (Right schedule) (Parse.fromString "* * * * *")

    Heck.it t "accepts extra spaces" $ do
      schedule <- ScheduleSpec.new [] [] [] [] []
      Heck.assertEq t (Right schedule) (Parse.fromString "  *  *  *  *  *  ")

    Heck.it t "accepts numbers" $ do
      schedule <- ScheduleSpec.new [[4]] [[3]] [[2]] [[1]] [[0]]
      Heck.assertEq t (Right schedule) (Parse.fromString "4 3 2 1 0")

    Heck.it t "accepts ranges" $ do
      schedule <- ScheduleSpec.new [[8, 9]] [[6, 7]] [[4, 5]] [[2, 3]] [[0, 1]]
      Heck.assertEq t (Right schedule) (Parse.fromString "8-9 6-7 4-5 2-3 0-1")

    Heck.it t "accepts choices" $ do
      schedule <- ScheduleSpec.new [[8], [9]] [[6], [7]] [[4], [5]] [[2], [3]] [[0], [1]]
      Heck.assertEq t (Right schedule) (Parse.fromString "8,9 6,7 4,5 2,3 0,1")

    Heck.describe t "minute" $ do
      Heck.it t "accepts a number" $ do
        schedule <- ScheduleSpec.new [[0]] [] [] [] []
        Heck.assertEq t (Right schedule) (Parse.fromString "0 * * * *")

      Heck.it t "accepts a range" $ do
        schedule <- ScheduleSpec.new [[0, 1]] [] [] [] []
        Heck.assertEq t (Right schedule) (Parse.fromString "0-1 * * * *")

      Heck.it t "accepts a choice" $ do
        schedule <- ScheduleSpec.new [[0], [1]] [] [] [] []
        Heck.assertEq t (Right schedule) (Parse.fromString "0,1 * * * *")

      Heck.it t "rejects two wildcards" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "*,* * * * *"))

      Heck.it t "rejects a wildcard and a number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "*,0 * * * *"))

      Heck.it t "rejects a wildcard and a range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "*,0-0 * * * *"))

      Heck.it t "rejects an out of bounds number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "60 * * * *"))

      Heck.it t "rejects an out of bounds number as part of a choice" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "0,60 * * * *"))

      Heck.it t "rejects an out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "60-61 * * * *"))

      Heck.it t "rejects a half out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "0-60 * * * *"))

      Heck.it t "rejects a backwards range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "1-0 * * * *"))

    Heck.describe t "hour" $ do
      Heck.it t "accepts a number" $ do
        schedule <- ScheduleSpec.new [] [[0]] [] [] []
        Heck.assertEq t (Right schedule) (Parse.fromString "* 0 * * *")

      Heck.it t "accepts a range" $ do
        schedule <- ScheduleSpec.new [] [[0, 1]] [] [] []
        Heck.assertEq t (Right schedule) (Parse.fromString "* 0-1 * * *")

      Heck.it t "accepts a choice" $ do
        schedule <- ScheduleSpec.new [] [[0], [1]] [] [] []
        Heck.assertEq t (Right schedule) (Parse.fromString "* 0,1 * * *")

      Heck.it t "rejects two wildcards" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* *,* * * *"))

      Heck.it t "rejects a wildcard and a number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* *,0 * * *"))

      Heck.it t "rejects a wildcard and a range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* *,0-0 * * *"))

      Heck.it t "rejects an out of bounds number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* 24 * * *"))

      Heck.it t "rejects an out of bounds number as part of a choice" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* 0,24 * * *"))

      Heck.it t "rejects an out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* 24-25 * * *"))

      Heck.it t "rejects a half out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* 0-24 * * *"))

      Heck.it t "rejects a backwards range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* 1-0 * * *"))

    Heck.describe t "day" $ do
      Heck.it t "accepts a number" $ do
        schedule <- ScheduleSpec.new [] [] [[1]] [] []
        Heck.assertEq t (Right schedule) (Parse.fromString "* * 1 * *")

      Heck.it t "accepts a range" $ do
        schedule <- ScheduleSpec.new [] [] [[1, 2]] [] []
        Heck.assertEq t (Right schedule) (Parse.fromString "* * 1-2 * *")

      Heck.it t "accepts a choice" $ do
        schedule <- ScheduleSpec.new [] [] [[1], [2]] [] []
        Heck.assertEq t (Right schedule) (Parse.fromString "* * 1,2 * *")

      Heck.it t "rejects two wildcards" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * *,* * *"))

      Heck.it t "rejects a wildcard and a number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * *,1 * *"))

      Heck.it t "rejects a wildcard and a range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * *,1-1 * *"))

      Heck.it t "rejects an out of bounds number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * 32 * *"))

      Heck.it t "rejects an out of bounds number as part of a choice" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * 1,32 * *"))

      Heck.it t "rejects an out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * 32-33 * *"))

      Heck.it t "rejects a half out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * 1-32 * *"))

      Heck.it t "rejects a backwards range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * 2-1 * *"))

    Heck.describe t "month" $ do
      Heck.it t "accepts a number" $ do
        schedule <- ScheduleSpec.new [] [] [] [[1]] []
        Heck.assertEq t (Right schedule) (Parse.fromString "* * * 1 *")

      Heck.it t "accepts a range" $ do
        schedule <- ScheduleSpec.new [] [] [] [[1, 2]] []
        Heck.assertEq t (Right schedule) (Parse.fromString "* * * 1-2 *")

      Heck.it t "accepts a choice" $ do
        schedule <- ScheduleSpec.new [] [] [] [[1], [2]] []
        Heck.assertEq t (Right schedule) (Parse.fromString "* * * 1,2 *")

      Heck.it t "rejects two wildcards" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * *,* *"))

      Heck.it t "rejects a wildcard and a number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * *,1 *"))

      Heck.it t "rejects a wildcard and a range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * *,1-1 *"))

      Heck.it t "rejects an out of bounds number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * 13 *"))

      Heck.it t "rejects an out of bounds number as part of a choice" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * 1,13 *"))

      Heck.it t "rejects an out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * 13-14 *"))

      Heck.it t "rejects a half out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * 1-13 *"))

      Heck.it t "rejects a backwards range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * 2-1 *"))

    Heck.describe t "weekday" $ do
      Heck.it t "accepts a number" $ do
        schedule <- ScheduleSpec.new [] [] [] [] [[0]]
        Heck.assertEq t (Right schedule) (Parse.fromString "* * * * 0")

      Heck.it t "accepts a range" $ do
        schedule <- ScheduleSpec.new [] [] [] [] [[0, 1]]
        Heck.assertEq t (Right schedule) (Parse.fromString "* * * * 0-1")

      Heck.it t "accepts a choice" $ do
        schedule <- ScheduleSpec.new [] [] [] [] [[0], [1]]
        Heck.assertEq t (Right schedule) (Parse.fromString "* * * * 0,1")

      Heck.it t "rejects two wildcards" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * * *,*"))

      Heck.it t "rejects a wildcard and a number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * * *,0"))

      Heck.it t "rejects a wildcard and a range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * * *,0-0"))

      Heck.it t "rejects an out of bounds number" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * * 7"))

      Heck.it t "rejects an out of bounds number as part of a choice" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * * 0,7"))

      Heck.it t "rejects an out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * * 7-8"))

      Heck.it t "rejects a half out of bounds range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * * 0-7"))

      Heck.it t "rejects a backwards range" $ do
        Heck.assertEq t True (Either.isLeft (Parse.fromString "* * * * 1-0"))
