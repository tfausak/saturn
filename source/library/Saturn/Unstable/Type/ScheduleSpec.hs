module Saturn.Unstable.Type.ScheduleSpec where

import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Heck
import qualified Saturn.Unstable.Type.DaySpec as DaySpec
import qualified Saturn.Unstable.Type.HourSpec as HourSpec
import qualified Saturn.Unstable.Type.MinuteSpec as MinuteSpec
import qualified Saturn.Unstable.Type.MonthSpec as MonthSpec
import qualified Saturn.Unstable.Type.Schedule as Schedule
import qualified Saturn.Unstable.Type.WeekdaySpec as WeekdaySpec
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Heck.Test IO n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Type.Schedule" $ do
  Heck.it t "round trips"
    . QuickCheck.quickCheck
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Schedule.parsec "" (Builder.toLazyText $ Schedule.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Schedule.Schedule
arbitrary =
  Schedule.Schedule
    <$> MinuteSpec.arbitrary
    <*> HourSpec.arbitrary
    <*> DaySpec.arbitrary
    <*> MonthSpec.arbitrary
    <*> WeekdaySpec.arbitrary

shrink :: Schedule.Schedule -> [Schedule.Schedule]
shrink schedule =
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
      MinuteSpec.shrink
      HourSpec.shrink
      DaySpec.shrink
      MonthSpec.shrink
      WeekdaySpec.shrink
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

new ::
  (MonadFail m) =>
  [[Word.Word8]] ->
  [[Word.Word8]] ->
  [[Word.Word8]] ->
  [[Word.Word8]] ->
  [[Word.Word8]] ->
  m Schedule.Schedule
new minutes hours days months weekdays =
  Schedule.Schedule
    <$> MinuteSpec.new minutes
    <*> HourSpec.new hours
    <*> DaySpec.new days
    <*> MonthSpec.new months
    <*> WeekdaySpec.new weekdays
