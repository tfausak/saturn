import qualified Saturn.Unstable.Extra.IntSpec
import qualified Saturn.Unstable.Extra.OrdSpec
import qualified Saturn.Unstable.Extra.ParsecSpec
import qualified Saturn.Unstable.Extra.TimeSpec
import qualified Saturn.Unstable.Extra.TupleSpec
import qualified Saturn.Unstable.MatchSpec
import qualified Saturn.Unstable.ParseSpec
import qualified Saturn.Unstable.RenderSpec
import qualified Saturn.Unstable.Type.DaySpec
import qualified Saturn.Unstable.Type.ElementSpec
import qualified Saturn.Unstable.Type.FieldSpec
import qualified Saturn.Unstable.Type.HourSpec
import qualified Saturn.Unstable.Type.MinuteSpec
import qualified Saturn.Unstable.Type.MonthSpec
import qualified Saturn.Unstable.Type.NumberSpec
import qualified Saturn.Unstable.Type.RangeSpec
import qualified Saturn.Unstable.Type.ScheduleSpec
import qualified Saturn.Unstable.Type.WeekdaySpec
import qualified Saturn.Unstable.Type.WildcardSpec
import qualified SaturnSpec
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ Hspec.parallel spec

spec :: Hspec.Spec
spec = do
  Saturn.Unstable.Extra.IntSpec.spec
  Saturn.Unstable.Extra.OrdSpec.spec
  Saturn.Unstable.Extra.ParsecSpec.spec
  Saturn.Unstable.Extra.TimeSpec.spec
  Saturn.Unstable.Extra.TupleSpec.spec
  Saturn.Unstable.MatchSpec.spec
  Saturn.Unstable.ParseSpec.spec
  Saturn.Unstable.RenderSpec.spec
  Saturn.Unstable.Type.DaySpec.spec
  Saturn.Unstable.Type.ElementSpec.spec
  Saturn.Unstable.Type.FieldSpec.spec
  Saturn.Unstable.Type.HourSpec.spec
  Saturn.Unstable.Type.MinuteSpec.spec
  Saturn.Unstable.Type.MonthSpec.spec
  Saturn.Unstable.Type.NumberSpec.spec
  Saturn.Unstable.Type.RangeSpec.spec
  Saturn.Unstable.Type.ScheduleSpec.spec
  Saturn.Unstable.Type.WeekdaySpec.spec
  Saturn.Unstable.Type.WildcardSpec.spec
  SaturnSpec.spec
