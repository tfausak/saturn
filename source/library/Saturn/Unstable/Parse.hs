module Saturn.Unstable.Parse where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Saturn.Unstable.Type.Schedule as Schedule
import qualified Text.Parsec as Parsec

-- | Parses a lazy 'LazyText.Text' value into a 'Schedule.Schedule'. See
-- 'fromText' for details.
fromLazyText :: LazyText.Text -> Either Parsec.ParseError Schedule.Schedule
fromLazyText = Parsec.parse parsec ""

-- | Parses a 'String' into a 'Schedule.Schedule'. See 'fromText' for details.
fromString :: String -> Either Parsec.ParseError Schedule.Schedule
fromString = Parsec.parse parsec ""

-- | Parses a strict 'Text.Text' value into a 'Schedule.Schedule'. The input
-- should have five fields, each separated by at least one space. Leading and
-- trailing spaces are allowed.
fromText :: Text.Text -> Either Parsec.ParseError Schedule.Schedule
fromText = Parsec.parse parsec ""

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Schedule.Schedule
parsec =
  Parsec.skipMany (Parsec.char ' ')
    *> Schedule.parsec
    <* Parsec.skipMany (Parsec.char ' ')
    <* Parsec.eof
