module Saturn.Unstable.Type.MinuteSpec where

import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Heck
import qualified Saturn.Unstable.Type.FieldSpec as FieldSpec
import qualified Saturn.Unstable.Type.Minute as Minute
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Heck.Test IO n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Type.Minute" $ do
  Heck.it t "round trips"
    . QuickCheck.quickCheck
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Minute.parsec "" (Builder.toLazyText $ Minute.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Minute.Minute
arbitrary = QuickCheck.suchThatMap FieldSpec.arbitrary Minute.fromField

shrink :: Minute.Minute -> [Minute.Minute]
shrink = Maybe.mapMaybe Minute.fromField . FieldSpec.shrink . Minute.toField

new :: (MonadFail m) => [[Word.Word8]] -> m Minute.Minute
new xs = do
  field <- FieldSpec.new xs
  maybe (fail $ "invalid Minute: " <> show xs) pure $ Minute.fromField field
