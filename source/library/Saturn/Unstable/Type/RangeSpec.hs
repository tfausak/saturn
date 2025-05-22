module Saturn.Unstable.Type.RangeSpec where

import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Extra.Tuple as Tuple
import qualified Saturn.Unstable.Type.Number as Number
import qualified Saturn.Unstable.Type.NumberSpec as NumberSpec
import qualified Saturn.Unstable.Type.Range as Range
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Type.Range" $ do
  Hspec.it "round trips"
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Range.parsec "" (Builder.toLazyText $ Range.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Range.Range
arbitrary =
  QuickCheck.suchThatMap
    (QuickCheck.liftArbitrary2 NumberSpec.arbitrary NumberSpec.arbitrary)
    Range.fromTuple

shrink :: Range.Range -> [Range.Range]
shrink =
  Maybe.mapMaybe Range.fromTuple
    . QuickCheck.liftShrink2 NumberSpec.shrink NumberSpec.shrink
    . Range.toTuple

new :: (MonadFail m) => (Word.Word8, Word.Word8) -> m Range.Range
new tuple =
  maybe (fail $ "invalid Range: " <> show tuple) pure
    . Range.fromTuple
    $ Tuple.mapBoth Number.fromWord8 tuple
