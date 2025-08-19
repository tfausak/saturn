module Saturn.Unstable.Type.MonthSpec where

import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Heck
import qualified Saturn.Unstable.Type.FieldSpec as FieldSpec
import qualified Saturn.Unstable.Type.Month as Month
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Heck.Test IO n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Type.Month" $ do
  Heck.it t "round trips"
    . QuickCheck.quickCheck
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Month.parsec "" (Builder.toLazyText $ Month.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Month.Month
arbitrary = QuickCheck.suchThatMap FieldSpec.arbitrary Month.fromField

shrink :: Month.Month -> [Month.Month]
shrink = Maybe.mapMaybe Month.fromField . FieldSpec.shrink . Month.toField

new :: (MonadFail m) => [[Word.Word8]] -> m Month.Month
new xs = do
  field <- FieldSpec.new xs
  maybe (fail $ "invalid Month: " <> show xs) pure $ Month.fromField field
