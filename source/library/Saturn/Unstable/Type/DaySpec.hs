module Saturn.Unstable.Type.DaySpec where

import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Heck
import qualified Saturn.Unstable.Type.Day as Day
import qualified Saturn.Unstable.Type.FieldSpec as FieldSpec
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Heck.Test IO n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Type.Day" $ do
  Heck.it t "round trips"
    . QuickCheck.quickCheck
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Day.parsec "" (Builder.toLazyText $ Day.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Day.Day
arbitrary = QuickCheck.suchThatMap FieldSpec.arbitrary Day.fromField

shrink :: Day.Day -> [Day.Day]
shrink = Maybe.mapMaybe Day.fromField . FieldSpec.shrink . Day.toField

new :: (MonadFail m) => [[Word.Word8]] -> m Day.Day
new xs = do
  field <- FieldSpec.new xs
  maybe (fail $ "invalid Day: " <> show xs) pure $ Day.fromField field
