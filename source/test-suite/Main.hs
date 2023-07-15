import qualified Saturn.Unstable.Extra.IntSpec
import qualified Saturn.Unstable.Extra.OrdSpec
import qualified Saturn.Unstable.Extra.ParsecSpec
import qualified Saturn.Unstable.Extra.TimeSpec
import qualified Saturn.Unstable.Extra.TupleSpec
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
  SaturnSpec.spec
