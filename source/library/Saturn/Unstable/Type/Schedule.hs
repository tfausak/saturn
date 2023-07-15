module Saturn.Unstable.Type.Schedule where

import qualified Data.Text.Lazy.Builder as Builder
import qualified Saturn.Unstable.Type.Day as Day
import qualified Saturn.Unstable.Type.Hour as Hour
import qualified Saturn.Unstable.Type.Minute as Minute
import qualified Saturn.Unstable.Type.Month as Month
import qualified Saturn.Unstable.Type.Weekday as Weekday
import qualified Text.Parsec as Parsec

data Schedule = Schedule
  { minute :: Minute.Minute,
    hour :: Hour.Hour,
    day :: Day.Day,
    month :: Month.Month,
    weekday :: Weekday.Weekday
  }
  deriving (Eq, Show)

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Schedule
parsec =
  Schedule
    <$> Minute.parsec
    <* Parsec.skipMany1 (Parsec.char ' ')
    <*> Hour.parsec
    <* Parsec.skipMany1 (Parsec.char ' ')
    <*> Day.parsec
    <* Parsec.skipMany1 (Parsec.char ' ')
    <*> Month.parsec
    <* Parsec.skipMany1 (Parsec.char ' ')
    <*> Weekday.parsec

toBuilder :: Schedule -> Builder.Builder
toBuilder schedule =
  Minute.toBuilder (minute schedule)
    <> Builder.singleton ' '
    <> Hour.toBuilder (hour schedule)
    <> Builder.singleton ' '
    <> Day.toBuilder (day schedule)
    <> Builder.singleton ' '
    <> Month.toBuilder (month schedule)
    <> Builder.singleton ' '
    <> Weekday.toBuilder (weekday schedule)
