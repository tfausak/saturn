module Saturn.Unstable.Match where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Time as Time
import qualified Saturn.Unstable.Extra.Int as Int
import qualified Saturn.Unstable.Extra.Time as Time
import qualified Saturn.Unstable.Type.Day as Day
import qualified Saturn.Unstable.Type.Field as Field
import qualified Saturn.Unstable.Type.Hour as Hour
import qualified Saturn.Unstable.Type.Minute as Minute
import qualified Saturn.Unstable.Type.Month as Month
import qualified Saturn.Unstable.Type.Schedule as Schedule
import qualified Saturn.Unstable.Type.Weekday as Weekday
import qualified Saturn.Unstable.Type.Wildcard as Wildcard

-- | Returns 'True' if the given 'Time.UTCTime' matches the given
-- 'Schedule.Schedule', otherwise returns 'False'.
isMatch :: Time.UTCTime -> Schedule.Schedule -> Bool
isMatch utcTime schedule = Maybe.fromMaybe False $ do
  let time = Time.pastMidnight $ Time.utctDayTime utcTime
  minute <- Int.toWord8 $ Time.todMin time
  Monad.guard . Minute.isMatch minute $ Schedule.minute schedule

  hour <- Int.toWord8 $ Time.todHour time
  Monad.guard . Hour.isMatch hour $ Schedule.hour schedule

  let date = Time.utctDay utcTime
  let (_, monthOfYear, dayOfMonth) = Time.toGregorian date
  month <- Int.toWord8 monthOfYear
  Monad.guard . Month.isMatch month $ Schedule.month schedule

  day <- Int.toWord8 dayOfMonth
  let dayMatches = Day.isMatch day $ Schedule.day schedule
  let weekday = Time.dayOfWeekToWord8 $ Time.dayOfWeek date
  let weekdayMatches = Weekday.isMatch weekday $ Schedule.weekday schedule
  Monad.guard $
    if dayIsWildcard schedule || weekdayIsWildcard schedule
      then dayMatches && weekdayMatches
      else dayMatches || weekdayMatches

  pure True

dayIsWildcard :: Schedule.Schedule -> Bool
dayIsWildcard = Field.isWildcard . Day.toField . Schedule.day

weekdayIsWildcard :: Schedule.Schedule -> Bool
weekdayIsWildcard = Field.isWildcard . Weekday.toField . Schedule.weekday

-- | Looks for the first time after the given 'Time.UTCTime' that matches the
-- given 'Schedule.Schedule'. Returns 'Nothing' if the 'Schedule.Schedule' only
-- matches dates that cannot happen, like February 30th.
nextMatch :: Time.UTCTime -> Schedule.Schedule -> Maybe Time.UTCTime
nextMatch utcTime schedule =
  if dayIsWildcard schedule || weekdayIsWildcard schedule
    then incompleteNextMatch utcTime schedule
    else do
      let wildcard = Field.fromEither . Left $ Wildcard.fromUnit ()
      day <- Day.fromField wildcard
      weekday <- Weekday.fromField wildcard
      Maybe.listToMaybe . List.sort $
        Maybe.catMaybes
          [ incompleteNextMatch utcTime schedule {Schedule.day = day},
            incompleteNextMatch utcTime schedule {Schedule.weekday = weekday}
          ]

incompleteNextMatch :: Time.UTCTime -> Schedule.Schedule -> Maybe Time.UTCTime
incompleteNextMatch utcTime schedule = Maybe.listToMaybe $ do
  let oldDate = Time.utctDay utcTime
  let (oldYear, _, _) = Time.toGregorian oldDate
  year <- [oldYear .. oldYear + 8]
  month <- fmap Int.fromWord8 . Set.toAscList . Month.expand $ Schedule.month schedule
  day <- fmap Int.fromWord8 . Set.toAscList . Day.expand $ Schedule.day schedule
  date <- Maybe.maybeToList $ Time.fromGregorianValid year month day
  Monad.guard $ date >= oldDate
  Monad.guard
    . Set.member (Time.dayOfWeekToWord8 $ Time.dayOfWeek date)
    . Weekday.expand
    $ Schedule.weekday schedule
  hour <- fmap Int.fromWord8 . Set.toAscList . Hour.expand $ Schedule.hour schedule
  minute <- fmap Int.fromWord8 . Set.toAscList . Minute.expand $ Schedule.minute schedule
  time <-
    fmap Time.sinceMidnight
      . Maybe.maybeToList
      $ Time.makeTimeOfDayValid hour minute 0
  Monad.when (date == oldDate) . Monad.guard $ time > Time.utctDayTime utcTime
  pure Time.UTCTime {Time.utctDay = date, Time.utctDayTime = time}
