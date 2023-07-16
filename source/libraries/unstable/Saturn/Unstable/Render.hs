module Saturn.Unstable.Render where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Saturn.Unstable.Type.Schedule as Schedule

-- | Renders a 'Schedule.Schedule' into a lazy 'LazyText.Text' value. See
-- 'toText' for details.
toLazyText :: Schedule.Schedule -> LazyText.Text
toLazyText = Builder.toLazyText . Schedule.toBuilder

-- | Renders a 'Schedule.Schedule' into a 'String' value. See 'toText' for
-- details.
toString :: Schedule.Schedule -> String
toString = LazyText.unpack . toLazyText

-- | Renders a 'Schedule.Schedule' into a strict 'Text.Text' value. The output
-- will have five fields, each separated by a single space.
toText :: Schedule.Schedule -> Text.Text
toText = LazyText.toStrict . toLazyText
