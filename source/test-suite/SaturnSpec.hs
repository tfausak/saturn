module SaturnSpec where

import qualified Data.Either as Either
import qualified Data.Fixed as Fixed
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Data.Word as Word
import qualified Saturn
import qualified Saturn.Unstable.Extra.Tuple as Tuple
import qualified Saturn.Unstable.Type.Day as Day
import qualified Saturn.Unstable.Type.Element as Element
import qualified Saturn.Unstable.Type.Field as Field
import qualified Saturn.Unstable.Type.Hour as Hour
import qualified Saturn.Unstable.Type.Minute as Minute
import qualified Saturn.Unstable.Type.Month as Month
import qualified Saturn.Unstable.Type.Number as Number
import qualified Saturn.Unstable.Type.Range as Range
import qualified Saturn.Unstable.Type.Schedule as Schedule
import qualified Saturn.Unstable.Type.Weekday as Weekday
import qualified Saturn.Unstable.Type.Wildcard as Wildcard
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Saturn" $ do
  Hspec.describe "round trips" $ do
    Hspec.it "through string"
      . QuickCheck.forAllShrink arbitrarySchedule shrinkSchedule
      $ \schedule ->
        Saturn.fromString (Saturn.toString schedule) `Hspec.shouldBe` Right schedule

    Hspec.it "through strict text"
      . QuickCheck.forAllShrink arbitrarySchedule shrinkSchedule
      $ \schedule ->
        Saturn.fromText (Saturn.toText schedule) `Hspec.shouldBe` Right schedule

    Hspec.it "through lazy text"
      . QuickCheck.forAllShrink arbitrarySchedule shrinkSchedule
      $ \schedule ->
        Saturn.fromLazyText (Saturn.toLazyText schedule) `Hspec.shouldBe` Right schedule

  Hspec.describe "fromString" $ do
    Hspec.it "accepts wildcards" $ do
      schedule <- newSchedule [] [] [] [] []
      Saturn.fromString "* * * * *" `Hspec.shouldBe` Right schedule

    Hspec.it "accepts extra spaces" $ do
      schedule <- newSchedule [] [] [] [] []
      Saturn.fromString "  *  *  *  *  *  " `Hspec.shouldBe` Right schedule

    Hspec.it "accepts numbers" $ do
      schedule <- newSchedule [[4]] [[3]] [[2]] [[1]] [[0]]
      Saturn.fromString "4 3 2 1 0" `Hspec.shouldBe` Right schedule

    Hspec.it "accepts ranges" $ do
      schedule <- newSchedule [[8, 9]] [[6, 7]] [[4, 5]] [[2, 3]] [[0, 1]]
      Saturn.fromString "8-9 6-7 4-5 2-3 0-1" `Hspec.shouldBe` Right schedule

    Hspec.it "accepts choices" $ do
      schedule <- newSchedule [[8], [9]] [[6], [7]] [[4], [5]] [[2], [3]] [[0], [1]]
      Saturn.fromString "8,9 6,7 4,5 2,3 0,1" `Hspec.shouldBe` Right schedule

    Hspec.describe "minute" $ do
      Hspec.it "accepts a number" $ do
        schedule <- newSchedule [[0]] [] [] [] []
        Saturn.fromString "0 * * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- newSchedule [[0, 1]] [] [] [] []
        Saturn.fromString "0-1 * * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- newSchedule [[0], [1]] [] [] [] []
        Saturn.fromString "0,1 * * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Saturn.fromString "*,* * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Saturn.fromString "*,0 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Saturn.fromString "*,0-0 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Saturn.fromString "60 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Saturn.fromString "0,60 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Saturn.fromString "60-61 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Saturn.fromString "0-60 * * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Saturn.fromString "1-0 * * * *" `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "hour" $ do
      Hspec.it "accepts a number" $ do
        schedule <- newSchedule [] [[0]] [] [] []
        Saturn.fromString "* 0 * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- newSchedule [] [[0, 1]] [] [] []
        Saturn.fromString "* 0-1 * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- newSchedule [] [[0], [1]] [] [] []
        Saturn.fromString "* 0,1 * * *" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Saturn.fromString "* *,* * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Saturn.fromString "* *,0 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Saturn.fromString "* *,0-0 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Saturn.fromString "* 24 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Saturn.fromString "* 0,24 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Saturn.fromString "* 24-25 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Saturn.fromString "* 0-24 * * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Saturn.fromString "* 1-0 * * *" `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "day" $ do
      Hspec.it "accepts a number" $ do
        schedule <- newSchedule [] [] [[1]] [] []
        Saturn.fromString "* * 1 * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- newSchedule [] [] [[1, 2]] [] []
        Saturn.fromString "* * 1-2 * *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- newSchedule [] [] [[1], [2]] [] []
        Saturn.fromString "* * 1,2 * *" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Saturn.fromString "* * *,* * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Saturn.fromString "* * *,1 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Saturn.fromString "* * *,1-1 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Saturn.fromString "* * 32 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Saturn.fromString "* * 1,32 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Saturn.fromString "* * 32-33 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Saturn.fromString "* * 1-32 * *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Saturn.fromString "* * 2-1 * *" `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "month" $ do
      Hspec.it "accepts a number" $ do
        schedule <- newSchedule [] [] [] [[1]] []
        Saturn.fromString "* * * 1 *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- newSchedule [] [] [] [[1, 2]] []
        Saturn.fromString "* * * 1-2 *" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- newSchedule [] [] [] [[1], [2]] []
        Saturn.fromString "* * * 1,2 *" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Saturn.fromString "* * * *,* *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Saturn.fromString "* * * *,1 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Saturn.fromString "* * * *,1-1 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Saturn.fromString "* * * 13 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Saturn.fromString "* * * 1,13 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Saturn.fromString "* * * 13-14 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Saturn.fromString "* * * 1-13 *" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Saturn.fromString "* * * 2-1 *" `Hspec.shouldSatisfy` Either.isLeft

    Hspec.describe "weekday" $ do
      Hspec.it "accepts a number" $ do
        schedule <- newSchedule [] [] [] [] [[0]]
        Saturn.fromString "* * * * 0" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a range" $ do
        schedule <- newSchedule [] [] [] [] [[0, 1]]
        Saturn.fromString "* * * * 0-1" `Hspec.shouldBe` Right schedule

      Hspec.it "accepts a choice" $ do
        schedule <- newSchedule [] [] [] [] [[0], [1]]
        Saturn.fromString "* * * * 0,1" `Hspec.shouldBe` Right schedule

      Hspec.it "rejects two wildcards" $ do
        Saturn.fromString "* * * * *,*" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a number" $ do
        Saturn.fromString "* * * * *,0" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a wildcard and a range" $ do
        Saturn.fromString "* * * * *,0-0" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number" $ do
        Saturn.fromString "* * * * 7" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds number as part of a choice" $ do
        Saturn.fromString "* * * * 0,7" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects an out of bounds range" $ do
        Saturn.fromString "* * * * 7-8" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a half out of bounds range" $ do
        Saturn.fromString "* * * * 0-7" `Hspec.shouldSatisfy` Either.isLeft

      Hspec.it "rejects a backwards range" $ do
        Saturn.fromString "* * * * 1-0" `Hspec.shouldSatisfy` Either.isLeft

  Hspec.describe "toString" $ do
    Hspec.it "works with wildcards" $ do
      schedule <- newSchedule [] [] [] [] []
      Saturn.toString schedule `Hspec.shouldBe` "* * * * *"

    Hspec.it "works with numbers" $ do
      schedule <- newSchedule [[4]] [[3]] [[2]] [[1]] [[0]]
      Saturn.toString schedule `Hspec.shouldBe` "4 3 2 1 0"

    Hspec.it "works with ranges" $ do
      schedule <- newSchedule [[8, 9]] [[6, 7]] [[4, 5]] [[2, 3]] [[0, 1]]
      Saturn.toString schedule `Hspec.shouldBe` "8-9 6-7 4-5 2-3 0-1"

    Hspec.it "works with choices" $ do
      schedule <- newSchedule [[8], [9]] [[6], [7]] [[4], [5]] [[2], [3]] [[0], [1]]
      Saturn.toString schedule `Hspec.shouldBe` "8,9 6,7 4,5 2,3 0,1"

  Hspec.describe "isMatch" $ do
    Hspec.it "is always true with all wildcards"
      . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
      $ \utcTime -> do
        schedule <- newSchedule [] [] [] [] []
        schedule `Hspec.shouldSatisfy` Saturn.isMatch utcTime

    Hspec.it "is true when day or weekday matches" $ do
      s <- newSchedule [] [] [[5]] [] [[5]]
      t1 <- newUtcTime 1970 1 5 0 0 0
      s `Hspec.shouldSatisfy` Saturn.isMatch t1
      t2 <- newUtcTime 1970 1 2 0 0 0
      s `Hspec.shouldSatisfy` Saturn.isMatch t2

    Hspec.describe "minute" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [[5]] [] [] [] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withMinute 5 utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [[4, 5]] [] [] [] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withMinute 5 utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [[4], [5]] [] [] [] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withMinute 5 utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 1 1 0 5 0
        s <- newSchedule [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 1 1 0 6 0
        s <- newSchedule [[5]] [] [] [] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 1 1 0 5 0
        s <- newSchedule [[4, 5]] [] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 1 1 0 6 0
        s <- newSchedule [[4, 5]] [] [] [] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 1 1 0 5 0
        s <- newSchedule [[4], [5]] [] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 1 1 0 6 0
        s <- newSchedule [[4], [5]] [] [] [] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 1 1 0 5 6
        s <- newSchedule [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any hour" $ do
        t <- newUtcTime 1970 1 1 6 5 0
        s <- newSchedule [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any day" $ do
        t <- newUtcTime 1970 1 6 0 5 0
        s <- newSchedule [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any month" $ do
        t <- newUtcTime 1970 6 1 0 5 0
        s <- newSchedule [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

    Hspec.describe "hour" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [[5]] [] [] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withHour 5 utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [[4, 5]] [] [] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withHour 5 utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [[4], [5]] [] [] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withHour 5 utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 1 1 5 0 0
        s <- newSchedule [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 1 1 6 0 0
        s <- newSchedule [] [[5]] [] [] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 1 1 5 0 0
        s <- newSchedule [] [[4, 5]] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 1 1 6 0 0
        s <- newSchedule [] [[4, 5]] [] [] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 1 1 5 0 0
        s <- newSchedule [] [[4], [5]] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 1 1 6 0 0
        s <- newSchedule [] [[4], [5]] [] [] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 1 1 5 0 6
        s <- newSchedule [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any minute" $ do
        t <- newUtcTime 1970 1 1 5 6 0
        s <- newSchedule [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any day" $ do
        t <- newUtcTime 1970 1 6 5 0 0
        s <- newSchedule [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any month" $ do
        t <- newUtcTime 1970 6 1 5 0 0
        s <- newSchedule [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

    Hspec.describe "day" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [] [[5]] [] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withDayOfMonth 5 utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [] [[4, 5]] [] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withDayOfMonth 5 utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [] [[4], [5]] [] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withDayOfMonth 5 utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 1 5 0 0 0
        s <- newSchedule [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 1 6 0 0 0
        s <- newSchedule [] [] [[5]] [] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 1 5 0 0 0
        s <- newSchedule [] [] [[4, 5]] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 1 6 0 0 0
        s <- newSchedule [] [] [[4, 5]] [] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 1 5 0 0 0
        s <- newSchedule [] [] [[4], [5]] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 1 6 0 0 0
        s <- newSchedule [] [] [[4], [5]] [] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 1 5 0 0 6
        s <- newSchedule [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any minute" $ do
        t <- newUtcTime 1970 1 5 0 6 0
        s <- newSchedule [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any hour" $ do
        t <- newUtcTime 1970 1 5 6 0 0
        s <- newSchedule [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any month" $ do
        t <- newUtcTime 1970 6 5 0 0 0
        s <- newSchedule [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

    Hspec.describe "month" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [] [] [[5]] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withMonthOfYear 5 utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [] [] [[4, 5]] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withMonthOfYear 5 utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [] [] [[4], [5]] []
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withMonthOfYear 5 utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 5 1 0 0 0
        s <- newSchedule [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 6 1 0 0 0
        s <- newSchedule [] [] [] [[5]] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 5 1 0 0 0
        s <- newSchedule [] [] [] [[4, 5]] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 6 1 0 0 0
        s <- newSchedule [] [] [] [[4, 5]] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 5 1 0 0 0
        s <- newSchedule [] [] [] [[4], [5]] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 6 1 0 0 0
        s <- newSchedule [] [] [] [[4], [5]] []
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 5 1 0 0 6
        s <- newSchedule [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any minute" $ do
        t <- newUtcTime 1970 5 1 0 6 0
        s <- newSchedule [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any hour" $ do
        t <- newUtcTime 1970 5 1 6 0 0
        s <- newSchedule [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any day" $ do
        t <- newUtcTime 1970 5 6 0 0 0
        s <- newSchedule [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Saturn.isMatch t

    Hspec.describe "weekday" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [] [] [] [[5]]
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withDayOfWeek Time.Friday utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [] [] [] [[4, 5]]
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withDayOfWeek Time.Friday utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- newSchedule [] [] [] [] [[4], [5]]
          schedule `Hspec.shouldSatisfy` Saturn.isMatch (withDayOfWeek Time.Friday utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 1 2 0 0 0
        s <- newSchedule [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 1 3 0 0 0
        s <- newSchedule [] [] [] [] [[5]]
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 1 2 0 0 0
        s <- newSchedule [] [] [] [] [[4, 5]]
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 1 3 0 0 0
        s <- newSchedule [] [] [] [] [[4, 5]]
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 1 2 0 0 0
        s <- newSchedule [] [] [] [] [[4], [5]]
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 1 3 0 0 0
        s <- newSchedule [] [] [] [] [[4], [5]]
        s `Hspec.shouldNotSatisfy` Saturn.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 1 2 0 0 6
        s <- newSchedule [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any minute" $ do
        t <- newUtcTime 1970 1 2 0 6 0
        s <- newSchedule [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any hour" $ do
        t <- newUtcTime 1970 1 2 6 0 0
        s <- newSchedule [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Saturn.isMatch t

      Hspec.it "accepts any day" $ do
        t <- newUtcTime 1970 1 9 0 0 0
        s <- newSchedule [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Saturn.isMatch t

  Hspec.describe "nextMatch" $ do
    Hspec.it "succeeds with a leap day" $ do
      s <- newSchedule [[0]] [[0]] [[29]] [[2]] []
      t1 <- newUtcTime 1970 1 1 0 0 0
      t2 <- newUtcTime 1972 2 29 0 0 0
      Saturn.nextMatch t1 s `Hspec.shouldBe` Just t2

    Hspec.it "succeeds with the next leap day" $ do
      s <- newSchedule [[0]] [[0]] [[29]] [[2]] []
      t1 <- newUtcTime 1972 2 29 0 0 0
      t2 <- newUtcTime 1976 2 29 0 0 0
      Saturn.nextMatch t1 s `Hspec.shouldBe` Just t2

    Hspec.it "succeeds with the furthest leap day" $ do
      s <- newSchedule [[0]] [[0]] [[29]] [[2]] []
      t1 <- newUtcTime 1896 2 29 0 0 0
      t2 <- newUtcTime 1904 2 29 0 0 0
      Saturn.nextMatch t1 s `Hspec.shouldBe` Just t2

    Hspec.it "fails with an impossible date" $ do
      s <- newSchedule [[0]] [[0]] [[30]] [[2]] []
      t <- newUtcTime 1970 1 1 0 0 0
      Saturn.nextMatch t s `Hspec.shouldBe` Nothing

    Hspec.it "is always in the future"
      . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
      $ \t1 -> do
        schedule <- newSchedule [] [] [] [] []
        t2 <- maybe (fail "impossible") pure $ Saturn.nextMatch t1 schedule
        t2 `Hspec.shouldSatisfy` (>= t1)

    Hspec.it "always matches"
      . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
      $ \t1 -> do
        schedule <- newSchedule [] [] [] [] []
        t2 <- maybe (fail "impossible") pure $ Saturn.nextMatch t1 schedule
        schedule `Hspec.shouldSatisfy` Saturn.isMatch t2

withMinute :: Int -> Time.UTCTime -> Time.UTCTime
withMinute minute = overTimeOfDay $ \timeOfDay -> timeOfDay {Time.todMin = minute}

withHour :: Int -> Time.UTCTime -> Time.UTCTime
withHour hour = overTimeOfDay $ \timeOfDay -> timeOfDay {Time.todHour = hour}

overTimeOfDay :: (Time.TimeOfDay -> Time.TimeOfDay) -> Time.UTCTime -> Time.UTCTime
overTimeOfDay f utcTime =
  utcTime
    { Time.utctDayTime =
        Time.sinceMidnight . f . Time.pastMidnight $ Time.utctDayTime utcTime
    }

withDayOfMonth :: Time.DayOfMonth -> Time.UTCTime -> Time.UTCTime
withDayOfMonth dayOfMonth = overDay $ \day ->
  let (year, monthOfYear, _) = Time.toGregorian day
   in Time.fromGregorian year monthOfYear dayOfMonth

withMonthOfYear :: Time.MonthOfYear -> Time.UTCTime -> Time.UTCTime
withMonthOfYear monthOfYear = overDay $ \day ->
  let (year, _, dayOfMonth) = Time.toGregorian day
   in Time.fromGregorian year monthOfYear dayOfMonth

withDayOfWeek :: Time.DayOfWeek -> Time.UTCTime -> Time.UTCTime
withDayOfWeek dayOfWeek = overDay $ \day ->
  let fwt = Time.FirstWholeWeek
      dow = Time.Sunday
      (year, weekOfYear, _) = Time.toWeekCalendar fwt dow day
   in Time.fromWeekCalendar fwt dow year weekOfYear dayOfWeek

overDay :: (Time.Day -> Time.Day) -> Time.UTCTime -> Time.UTCTime
overDay f utcTime = utcTime {Time.utctDay = f $ Time.utctDay utcTime}

newUtcTime ::
  (MonadFail m) =>
  Time.Year ->
  Time.MonthOfYear ->
  Time.DayOfMonth ->
  Int ->
  Int ->
  Fixed.Pico ->
  m Time.UTCTime
newUtcTime year monthOfYear dayOfMonth hour minute second = do
  day <-
    maybe (fail "invalid Day") pure $
      Time.fromGregorianValid year monthOfYear dayOfMonth
  timeOfDay <-
    maybe (fail "invalid TimeOfDay") pure $
      Time.makeTimeOfDayValid hour minute second
  pure
    Time.UTCTime
      { Time.utctDay = day,
        Time.utctDayTime = Time.sinceMidnight timeOfDay
      }

arbitraryUtcTime :: QuickCheck.Gen Time.UTCTime
arbitraryUtcTime =
  Time.UTCTime
    <$> fmap Time.ModifiedJulianDay QuickCheck.arbitrary
    <*> fmap Time.picosecondsToDiffTime (QuickCheck.chooseInteger (0, 86400000000000000 - 1))

shrinkUtcTime :: Time.UTCTime -> [Time.UTCTime]
shrinkUtcTime =
  QuickCheck.shrinkMap
    ( \(d, t) ->
        Time.UTCTime
          { Time.utctDay = Time.ModifiedJulianDay d,
            Time.utctDayTime = Time.picosecondsToDiffTime t
          }
    )
    ( \x ->
        ( Time.toModifiedJulianDay $ Time.utctDay x,
          Time.diffTimeToPicoseconds $ Time.utctDayTime x
        )
    )

newSchedule ::
  (MonadFail m) =>
  [[Word.Word8]] ->
  [[Word.Word8]] ->
  [[Word.Word8]] ->
  [[Word.Word8]] ->
  [[Word.Word8]] ->
  m Schedule.Schedule
newSchedule minutes hours days months weekdays =
  Schedule.Schedule
    <$> newFieldWith "Minute" Minute.fromField minutes
    <*> newFieldWith "Hour" Hour.fromField hours
    <*> newFieldWith "Day" Day.fromField days
    <*> newFieldWith "Month" Month.fromField months
    <*> newFieldWith "Weekday" Weekday.fromField weekdays

newFieldWith ::
  (MonadFail m) =>
  String ->
  (Field.Field -> Maybe a) ->
  [[Word.Word8]] ->
  m a
newFieldWith name fromField xs = do
  field <- newField xs
  maybe (fail $ "invalid " <> name <> ": " <> show xs) pure $ fromField field

newField :: (MonadFail m) => [[Word.Word8]] -> m Field.Field
newField =
  fmap
    ( Field.fromEither
        . maybe (Left $ Wildcard.fromUnit ()) Right
        . NonEmpty.nonEmpty
    )
    . mapM newElement

newElement :: (MonadFail m) => [Word.Word8] -> m Element.Element
newElement xs = case xs of
  [x] -> pure . Element.fromEither . Right $ Number.fromWord8 x
  [x, y] -> Element.fromEither . Left <$> newRange (x, y)
  _ -> fail $ "invalid Element: " <> show xs

newRange :: (MonadFail m) => (Word.Word8, Word.Word8) -> m Range.Range
newRange tuple =
  maybe (fail $ "invalid Range: " <> show tuple) pure
    . Range.fromTuple
    $ Tuple.mapBoth Number.fromWord8 tuple

arbitrarySchedule :: QuickCheck.Gen Schedule.Schedule
arbitrarySchedule =
  Schedule.Schedule
    <$> arbitraryMinute
    <*> arbitraryHour
    <*> arbitraryDay
    <*> arbitraryMonth
    <*> arbitraryWeekday

arbitraryMinute :: QuickCheck.Gen Minute.Minute
arbitraryMinute = QuickCheck.suchThatMap arbitraryField Minute.fromField

arbitraryHour :: QuickCheck.Gen Hour.Hour
arbitraryHour = QuickCheck.suchThatMap arbitraryField Hour.fromField

arbitraryDay :: QuickCheck.Gen Day.Day
arbitraryDay = QuickCheck.suchThatMap arbitraryField Day.fromField

arbitraryMonth :: QuickCheck.Gen Month.Month
arbitraryMonth = QuickCheck.suchThatMap arbitraryField Month.fromField

arbitraryWeekday :: QuickCheck.Gen Weekday.Weekday
arbitraryWeekday = QuickCheck.suchThatMap arbitraryField Weekday.fromField

arbitraryField :: QuickCheck.Gen Field.Field
arbitraryField =
  Field.fromEither
    <$> QuickCheck.liftArbitrary2
      arbitraryWildcard
      (arbitraryNonEmpty arbitraryElement)

arbitraryWildcard :: QuickCheck.Gen Wildcard.Wildcard
arbitraryWildcard = pure $ Wildcard.fromUnit ()

arbitraryNonEmpty :: QuickCheck.Gen a -> QuickCheck.Gen (NonEmpty.NonEmpty a)
arbitraryNonEmpty g = (NonEmpty.:|) <$> g <*> QuickCheck.listOf g

arbitraryElement :: QuickCheck.Gen Element.Element
arbitraryElement =
  Element.fromEither
    <$> QuickCheck.liftArbitrary2
      arbitraryRange
      arbitraryNumber

arbitraryRange :: QuickCheck.Gen Range.Range
arbitraryRange =
  QuickCheck.suchThatMap
    (QuickCheck.liftArbitrary2 arbitraryNumber arbitraryNumber)
    Range.fromTuple

arbitraryNumber :: QuickCheck.Gen Number.Number
arbitraryNumber = Number.fromWord8 <$> QuickCheck.arbitrary

shrinkSchedule :: Schedule.Schedule -> [Schedule.Schedule]
shrinkSchedule schedule =
  ( \(minute, hour, day, month, weekday) ->
      Schedule.Schedule
        { Schedule.minute = minute,
          Schedule.hour = hour,
          Schedule.day = day,
          Schedule.month = month,
          Schedule.weekday = weekday
        }
  )
    <$> liftShrink5
      shrinkMinute
      shrinkHour
      shrinkDay
      shrinkMonth
      shrinkWeekday
      ( Schedule.minute schedule,
        Schedule.hour schedule,
        Schedule.day schedule,
        Schedule.month schedule,
        Schedule.weekday schedule
      )

liftShrink5 ::
  (t1 -> [t1]) ->
  (t2 -> [t2]) ->
  (t3 -> [t3]) ->
  (t4 -> [t4]) ->
  (t5 -> [t5]) ->
  (t1, t2, t3, t4, t5) ->
  [(t1, t2, t3, t4, t5)]
liftShrink5 f1 f2 f3 f4 f5 (x1, x2, x3, x4, x5) =
  (\((y1, y2), (y3, (y4, y5))) -> (y1, y2, y3, y4, y5))
    <$> QuickCheck.liftShrink2
      (QuickCheck.liftShrink2 f1 f2)
      (QuickCheck.liftShrink2 f3 (QuickCheck.liftShrink2 f4 f5))
      ((x1, x2), (x3, (x4, x5)))

shrinkMinute :: Minute.Minute -> [Minute.Minute]
shrinkMinute = Maybe.mapMaybe Minute.fromField . shrinkField . Minute.toField

shrinkHour :: Hour.Hour -> [Hour.Hour]
shrinkHour = Maybe.mapMaybe Hour.fromField . shrinkField . Hour.toField

shrinkDay :: Day.Day -> [Day.Day]
shrinkDay = Maybe.mapMaybe Day.fromField . shrinkField . Day.toField

shrinkMonth :: Month.Month -> [Month.Month]
shrinkMonth = Maybe.mapMaybe Month.fromField . shrinkField . Month.toField

shrinkWeekday :: Weekday.Weekday -> [Weekday.Weekday]
shrinkWeekday = Maybe.mapMaybe Weekday.fromField . shrinkField . Weekday.toField

shrinkField :: Field.Field -> [Field.Field]
shrinkField field =
  let xs = case Field.toEither field of
        Left _ -> []
        Right _ -> [Field.fromEither . Left $ Wildcard.fromUnit ()]
   in mappend xs
        . fmap Field.fromEither
        . QuickCheck.liftShrink2 shrinkWildcard (shrinkNonEmpty shrinkElement)
        $ Field.toEither field

shrinkWildcard :: Wildcard.Wildcard -> [Wildcard.Wildcard]
shrinkWildcard = fmap Wildcard.fromUnit . QuickCheck.shrink . Wildcard.toUnit

shrinkNonEmpty :: (a -> [a]) -> NonEmpty.NonEmpty a -> [NonEmpty.NonEmpty a]
shrinkNonEmpty f = Maybe.mapMaybe (NonEmpty.nonEmpty . f) . NonEmpty.toList

shrinkElement :: Element.Element -> [Element.Element]
shrinkElement element =
  let xs = case Element.toEither element of
        Left range ->
          let (lo, hi) = Range.toTuple range
           in fmap (Element.fromEither . Right) [lo, hi]
        Right _ -> []
   in mappend xs
        . fmap Element.fromEither
        . QuickCheck.liftShrink2 shrinkRange shrinkNumber
        $ Element.toEither element

shrinkRange :: Range.Range -> [Range.Range]
shrinkRange =
  Maybe.mapMaybe Range.fromTuple
    . QuickCheck.liftShrink2 shrinkNumber shrinkNumber
    . Range.toTuple

shrinkNumber :: Number.Number -> [Number.Number]
shrinkNumber = fmap Number.fromWord8 . QuickCheck.shrink . Number.toWord8
