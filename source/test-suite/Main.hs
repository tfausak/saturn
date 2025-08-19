import qualified Heck
import qualified Saturn.Unstable.ConstantSpec
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
import qualified Test.Hspec.Core.Spec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.parallel $ do
  Saturn.Unstable.ConstantSpec.spec heck
  Saturn.Unstable.Extra.IntSpec.spec heck
  Saturn.Unstable.Extra.OrdSpec.spec heck
  Saturn.Unstable.Extra.ParsecSpec.spec heck
  Saturn.Unstable.Extra.TimeSpec.spec heck
  Saturn.Unstable.Extra.TupleSpec.spec heck
  Saturn.Unstable.MatchSpec.spec heck
  Saturn.Unstable.ParseSpec.spec heck
  Saturn.Unstable.RenderSpec.spec heck
  Saturn.Unstable.Type.DaySpec.spec heck
  Saturn.Unstable.Type.ElementSpec.spec heck
  Saturn.Unstable.Type.FieldSpec.spec heck
  Saturn.Unstable.Type.HourSpec.spec heck
  Saturn.Unstable.Type.MinuteSpec.spec heck
  Saturn.Unstable.Type.MonthSpec.spec heck
  Saturn.Unstable.Type.NumberSpec.spec heck
  Saturn.Unstable.Type.RangeSpec.spec heck
  Saturn.Unstable.Type.ScheduleSpec.spec heck
  Saturn.Unstable.Type.WeekdaySpec.spec heck
  Saturn.Unstable.Type.WildcardSpec.spec heck
  SaturnSpec.spec heck

heck :: Heck.Test IO (Hspec.SpecM ())
heck =
  Heck.MkTest
    { Heck.assertFailure = fmap (\() -> error "impossible") . Hspec.expectationFailure,
      Heck.describe = Hspec.describe,
      Heck.it = Hspec.it
    }
