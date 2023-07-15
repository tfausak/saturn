-- | Saturn handles POSIX cron schedules, which are defined here:
-- <https://pubs.opengroup.org/onlinepubs/9699919799/utilities/crontab.html#tag_20_25_07>.
--
-- A cron schedule is specified with five fields, each separated with at least
-- one space. Each field can either be a wildcard (represented by an asterisk),
-- or it can be one or more elements separated by commas. Each element can
-- either be a number or a range, which is two numbers separated by a hyphen.
--
-- In order, the fields represent:
--
-- 1. Minute, between 0 and 59.
-- 2. Hour, between 0 and 23.
-- 3. Day, between 1 and 31.
-- 4. Month, between 1 and 12.
-- 5. Weekday, between 0 and 6 where 0 represents Sunday.
--
-- Here is a more graphical representation of the fields:
--
-- >  +--------- Minute
-- >  | +------- Hour
-- >  | | +----- Day
-- >  | | | +--- Month
-- >  | | | | +- Weekday
-- >  | | | | |
-- > "* * * * *"
--
-- To get started, use 'Parse.fromText' to parse a 'Schedule.Schedule'. Then
-- use 'Render.toText' to render it again. To see if the 'Schedule.Schedule'
-- matches a certain time, use 'Match.isMatch'. To get the next time that
-- matches a schedule, use 'Match.nextMatch'.
--
-- >>> :set -XOverloadedStrings
-- >>> let Right schedule = fromText "* * * * *"
-- >>> toText schedule
-- "* * * * *"
-- >>> let utcTime = Data.Time.UTCTime (Data.Time.fromGregorian 1970 1 1) 0
-- >>> isMatch utcTime schedule
-- True
-- >>> nextMatch utcTime schedule
-- Just 1970-01-01 00:01:00 UTC
module Saturn
  ( -- * Types
    Schedule.Schedule,

    -- * Parsing
    Parse.fromText,
    Parse.fromLazyText,
    Parse.fromString,

    -- * Rendering
    Render.toText,
    Render.toLazyText,
    Render.toString,

    -- * Matching
    Match.isMatch,
    Match.nextMatch,
  )
where

import qualified Saturn.Unstable.Match as Match
import qualified Saturn.Unstable.Parse as Parse
import qualified Saturn.Unstable.Render as Render
import qualified Saturn.Unstable.Type.Schedule as Schedule
