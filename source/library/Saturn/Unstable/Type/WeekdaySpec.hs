module Saturn.Unstable.Type.WeekdaySpec where

import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Type.FieldSpec as FieldSpec
import qualified Saturn.Unstable.Type.Weekday as Weekday
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Type.Weekday" $ do
  Hspec.it "round trips"
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Weekday.parsec "" (Builder.toLazyText $ Weekday.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Weekday.Weekday
arbitrary = QuickCheck.suchThatMap FieldSpec.arbitrary Weekday.fromField

shrink :: Weekday.Weekday -> [Weekday.Weekday]
shrink = Maybe.mapMaybe Weekday.fromField . FieldSpec.shrink . Weekday.toField

new :: (MonadFail m) => [[Word.Word8]] -> m Weekday.Weekday
new xs = do
  field <- FieldSpec.new xs
  maybe (fail $ "invalid Weekday: " <> show xs) pure $ Weekday.fromField field
