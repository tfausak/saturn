module Saturn.Unstable.Constant where

import qualified Data.Maybe as Maybe
import qualified Saturn.Unstable.Type.Day as Day
import qualified Saturn.Unstable.Type.Element as Element
import qualified Saturn.Unstable.Type.Field as Field
import qualified Saturn.Unstable.Type.Hour as Hour
import qualified Saturn.Unstable.Type.Minute as Minute
import qualified Saturn.Unstable.Type.Month as Month
import qualified Saturn.Unstable.Type.Number as Number
import qualified Saturn.Unstable.Type.Schedule as Schedule
import qualified Saturn.Unstable.Type.Weekday as Weekday
import qualified Saturn.Unstable.Type.Wildcard as Wildcard

-- | > "* * * * *"
everyMinute :: Schedule.Schedule
everyMinute = Maybe.fromMaybe (error "Saturn.Unstable.Constant.everyMinute") $ do
  let wildcard = Field.fromEither . Left $ Wildcard.fromUnit ()
  minute <- Minute.fromField wildcard
  hour <- Hour.fromField wildcard
  day <- Day.fromField wildcard
  month <- Month.fromField wildcard
  weekday <- Weekday.fromField wildcard
  pure
    Schedule.Schedule
      { Schedule.minute = minute,
        Schedule.hour = hour,
        Schedule.day = day,
        Schedule.month = month,
        Schedule.weekday = weekday
      }

-- | > "0 * * * *"
hourly :: Schedule.Schedule
hourly = Maybe.fromMaybe (error "Saturn.Unstable.Constant.hourly") $ do
  minute <-
    Minute.fromField
      . Field.fromEither
      . Right
      . pure
      . Element.fromEither
      . Right
      $ Number.fromWord8 0
  pure everyMinute {Schedule.minute = minute}

-- | > "0 0 * * *"
daily :: Schedule.Schedule
daily = Maybe.fromMaybe (error "Saturn.Unstable.Constant.daily") $ do
  hour <-
    Hour.fromField
      . Field.fromEither
      . Right
      . pure
      . Element.fromEither
      . Right
      $ Number.fromWord8 0
  pure hourly {Schedule.hour = hour}

-- | > "0 0 * * 0"
weekly :: Schedule.Schedule
weekly = Maybe.fromMaybe (error "Saturn.Unstable.Constant.weekly") $ do
  weekday <-
    Weekday.fromField
      . Field.fromEither
      . Right
      . pure
      . Element.fromEither
      . Right
      $ Number.fromWord8 0
  pure daily {Schedule.weekday = weekday}

-- | > "0 0 1 * *"
monthly :: Schedule.Schedule
monthly = Maybe.fromMaybe (error "Saturn.Unstable.Constant.monthly") $ do
  day <-
    Day.fromField
      . Field.fromEither
      . Right
      . pure
      . Element.fromEither
      . Right
      $ Number.fromWord8 1
  pure daily {Schedule.day = day}

-- | > "0 0 1 1 *"
yearly :: Schedule.Schedule
yearly = Maybe.fromMaybe (error "Saturn.Unstable.Constant.yearly") $ do
  month <-
    Month.fromField
      . Field.fromEither
      . Right
      . pure
      . Element.fromEither
      . Right
      $ Number.fromWord8 1
  pure monthly {Schedule.month = month}
