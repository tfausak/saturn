module Saturn.Unstable.Type.HourSpec where

import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Heck
import qualified Saturn.Unstable.Type.FieldSpec as FieldSpec
import qualified Saturn.Unstable.Type.Hour as Hour
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Heck.Test IO n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Type.Hour" $ do
  Heck.it t "round trips"
    . QuickCheck.quickCheck
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Hour.parsec "" (Builder.toLazyText $ Hour.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Hour.Hour
arbitrary = QuickCheck.suchThatMap FieldSpec.arbitrary Hour.fromField

shrink :: Hour.Hour -> [Hour.Hour]
shrink = Maybe.mapMaybe Hour.fromField . FieldSpec.shrink . Hour.toField

new :: (MonadFail m) => [[Word.Word8]] -> m Hour.Hour
new xs = do
  field <- FieldSpec.new xs
  maybe (fail $ "invalid Hour: " <> show xs) pure $ Hour.fromField field
