module Saturn.Unstable.MatchSpec where

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Saturn.Unstable.Match as Match
import qualified Saturn.Unstable.Type.ScheduleSpec as ScheduleSpec
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Match" $ do
  Hspec.describe "isMatch" $ do
    Hspec.it "is always true with all wildcards"
      . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
      $ \utcTime -> do
        schedule <- ScheduleSpec.new [] [] [] [] []
        schedule `Hspec.shouldSatisfy` Match.isMatch utcTime

    Hspec.it "is true when day or weekday matches" $ do
      s <- ScheduleSpec.new [] [] [[5]] [] [[5]]
      t1 <- newUtcTime 1970 1 5 0 0 0
      s `Hspec.shouldSatisfy` Match.isMatch t1
      t2 <- newUtcTime 1970 1 2 0 0 0
      s `Hspec.shouldSatisfy` Match.isMatch t2

    Hspec.describe "minute" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [[5]] [] [] [] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withMinute 5 utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [[4, 5]] [] [] [] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withMinute 5 utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [[4], [5]] [] [] [] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withMinute 5 utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 1 1 0 5 0
        s <- ScheduleSpec.new [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 1 1 0 6 0
        s <- ScheduleSpec.new [[5]] [] [] [] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 1 1 0 5 0
        s <- ScheduleSpec.new [[4, 5]] [] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 1 1 0 6 0
        s <- ScheduleSpec.new [[4, 5]] [] [] [] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 1 1 0 5 0
        s <- ScheduleSpec.new [[4], [5]] [] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 1 1 0 6 0
        s <- ScheduleSpec.new [[4], [5]] [] [] [] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 1 1 0 5 6
        s <- ScheduleSpec.new [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any hour" $ do
        t <- newUtcTime 1970 1 1 6 5 0
        s <- ScheduleSpec.new [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any day" $ do
        t <- newUtcTime 1970 1 6 0 5 0
        s <- ScheduleSpec.new [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any month" $ do
        t <- newUtcTime 1970 6 1 0 5 0
        s <- ScheduleSpec.new [[5]] [] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

    Hspec.describe "hour" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [[5]] [] [] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withHour 5 utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [[4, 5]] [] [] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withHour 5 utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [[4], [5]] [] [] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withHour 5 utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 1 1 5 0 0
        s <- ScheduleSpec.new [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 1 1 6 0 0
        s <- ScheduleSpec.new [] [[5]] [] [] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 1 1 5 0 0
        s <- ScheduleSpec.new [] [[4, 5]] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 1 1 6 0 0
        s <- ScheduleSpec.new [] [[4, 5]] [] [] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 1 1 5 0 0
        s <- ScheduleSpec.new [] [[4], [5]] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 1 1 6 0 0
        s <- ScheduleSpec.new [] [[4], [5]] [] [] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 1 1 5 0 6
        s <- ScheduleSpec.new [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any minute" $ do
        t <- newUtcTime 1970 1 1 5 6 0
        s <- ScheduleSpec.new [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any day" $ do
        t <- newUtcTime 1970 1 6 5 0 0
        s <- ScheduleSpec.new [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any month" $ do
        t <- newUtcTime 1970 6 1 5 0 0
        s <- ScheduleSpec.new [] [[5]] [] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

    Hspec.describe "day" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [] [[5]] [] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withDayOfMonth 5 utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [] [[4, 5]] [] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withDayOfMonth 5 utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [] [[4], [5]] [] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withDayOfMonth 5 utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 1 5 0 0 0
        s <- ScheduleSpec.new [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 1 6 0 0 0
        s <- ScheduleSpec.new [] [] [[5]] [] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 1 5 0 0 0
        s <- ScheduleSpec.new [] [] [[4, 5]] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 1 6 0 0 0
        s <- ScheduleSpec.new [] [] [[4, 5]] [] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 1 5 0 0 0
        s <- ScheduleSpec.new [] [] [[4], [5]] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 1 6 0 0 0
        s <- ScheduleSpec.new [] [] [[4], [5]] [] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 1 5 0 0 6
        s <- ScheduleSpec.new [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any minute" $ do
        t <- newUtcTime 1970 1 5 0 6 0
        s <- ScheduleSpec.new [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any hour" $ do
        t <- newUtcTime 1970 1 5 6 0 0
        s <- ScheduleSpec.new [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any month" $ do
        t <- newUtcTime 1970 6 5 0 0 0
        s <- ScheduleSpec.new [] [] [[5]] [] []
        s `Hspec.shouldSatisfy` Match.isMatch t

    Hspec.describe "month" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [] [] [[5]] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withMonthOfYear 5 utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [] [] [[4, 5]] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withMonthOfYear 5 utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [] [] [[4], [5]] []
          schedule `Hspec.shouldSatisfy` Match.isMatch (withMonthOfYear 5 utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 5 1 0 0 0
        s <- ScheduleSpec.new [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 6 1 0 0 0
        s <- ScheduleSpec.new [] [] [] [[5]] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 5 1 0 0 0
        s <- ScheduleSpec.new [] [] [] [[4, 5]] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 6 1 0 0 0
        s <- ScheduleSpec.new [] [] [] [[4, 5]] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 5 1 0 0 0
        s <- ScheduleSpec.new [] [] [] [[4], [5]] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 6 1 0 0 0
        s <- ScheduleSpec.new [] [] [] [[4], [5]] []
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 5 1 0 0 6
        s <- ScheduleSpec.new [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any minute" $ do
        t <- newUtcTime 1970 5 1 0 6 0
        s <- ScheduleSpec.new [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any hour" $ do
        t <- newUtcTime 1970 5 1 6 0 0
        s <- ScheduleSpec.new [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any day" $ do
        t <- newUtcTime 1970 5 6 0 0 0
        s <- ScheduleSpec.new [] [] [] [[5]] []
        s `Hspec.shouldSatisfy` Match.isMatch t

    Hspec.describe "weekday" $ do
      Hspec.it "is always true when a number matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [] [] [] [[5]]
          schedule `Hspec.shouldSatisfy` Match.isMatch (withDayOfWeek Time.Friday utcTime)

      Hspec.it "is always true when a range matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [] [] [] [[4, 5]]
          schedule `Hspec.shouldSatisfy` Match.isMatch (withDayOfWeek Time.Friday utcTime)

      Hspec.it "is always true when a choice matches"
        . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
        $ \utcTime -> do
          schedule <- ScheduleSpec.new [] [] [] [] [[4], [5]]
          schedule `Hspec.shouldSatisfy` Match.isMatch (withDayOfWeek Time.Friday utcTime)

      Hspec.it "is true when a number matches" $ do
        t <- newUtcTime 1970 1 2 0 0 0
        s <- ScheduleSpec.new [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a number does not match" $ do
        t <- newUtcTime 1970 1 3 0 0 0
        s <- ScheduleSpec.new [] [] [] [] [[5]]
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a range matches" $ do
        t <- newUtcTime 1970 1 2 0 0 0
        s <- ScheduleSpec.new [] [] [] [] [[4, 5]]
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a range does not match" $ do
        t <- newUtcTime 1970 1 3 0 0 0
        s <- ScheduleSpec.new [] [] [] [] [[4, 5]]
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "is true when a choice matches" $ do
        t <- newUtcTime 1970 1 2 0 0 0
        s <- ScheduleSpec.new [] [] [] [] [[4], [5]]
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "is false when a choice does not match" $ do
        t <- newUtcTime 1970 1 3 0 0 0
        s <- ScheduleSpec.new [] [] [] [] [[4], [5]]
        s `Hspec.shouldNotSatisfy` Match.isMatch t

      Hspec.it "accepts any second" $ do
        t <- newUtcTime 1970 1 2 0 0 6
        s <- ScheduleSpec.new [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any minute" $ do
        t <- newUtcTime 1970 1 2 0 6 0
        s <- ScheduleSpec.new [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any hour" $ do
        t <- newUtcTime 1970 1 2 6 0 0
        s <- ScheduleSpec.new [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Match.isMatch t

      Hspec.it "accepts any day" $ do
        t <- newUtcTime 1970 1 9 0 0 0
        s <- ScheduleSpec.new [] [] [] [] [[5]]
        s `Hspec.shouldSatisfy` Match.isMatch t

  Hspec.describe "nextMatch" $ do
    Hspec.it "succeeds with a leap day" $ do
      s <- ScheduleSpec.new [[0]] [[0]] [[29]] [[2]] []
      t1 <- newUtcTime 1970 1 1 0 0 0
      t2 <- newUtcTime 1972 2 29 0 0 0
      Match.nextMatch t1 s `Hspec.shouldBe` Just t2

    Hspec.it "succeeds with the next leap day" $ do
      s <- ScheduleSpec.new [[0]] [[0]] [[29]] [[2]] []
      t1 <- newUtcTime 1972 2 29 0 0 0
      t2 <- newUtcTime 1976 2 29 0 0 0
      Match.nextMatch t1 s `Hspec.shouldBe` Just t2

    Hspec.it "succeeds with the furthest leap day" $ do
      s <- ScheduleSpec.new [[0]] [[0]] [[29]] [[2]] []
      t1 <- newUtcTime 1896 2 29 0 0 0
      t2 <- newUtcTime 1904 2 29 0 0 0
      Match.nextMatch t1 s `Hspec.shouldBe` Just t2

    Hspec.it "fails with an impossible date" $ do
      s <- ScheduleSpec.new [[0]] [[0]] [[30]] [[2]] []
      t <- newUtcTime 1970 1 1 0 0 0
      Match.nextMatch t s `Hspec.shouldBe` Nothing

    Hspec.it "is always in the future"
      . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
      $ \t1 -> do
        schedule <- ScheduleSpec.new [] [] [] [] []
        t2 <- maybe (fail "impossible") pure $ Match.nextMatch t1 schedule
        t2 `Hspec.shouldSatisfy` (>= t1)

    Hspec.it "always matches"
      . QuickCheck.forAllShrink arbitraryUtcTime shrinkUtcTime
      $ \t1 -> do
        schedule <- ScheduleSpec.new [] [] [] [] []
        t2 <- maybe (fail "impossible") pure $ Match.nextMatch t1 schedule
        schedule `Hspec.shouldSatisfy` Match.isMatch t2

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
