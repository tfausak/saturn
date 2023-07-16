module Saturn.Unstable.Extra.Time where

import qualified Data.Time as Time
import qualified Data.Word as Word

dayOfWeekToWord8 :: Time.DayOfWeek -> Word.Word8
dayOfWeekToWord8 dayOfWeek = case dayOfWeek of
  Time.Sunday -> 0
  Time.Monday -> 1
  Time.Tuesday -> 2
  Time.Wednesday -> 3
  Time.Thursday -> 4
  Time.Friday -> 5
  Time.Saturday -> 6
