module Saturn.Unstable.Type.FieldSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Heck
import qualified Saturn.Unstable.Type.ElementSpec as ElementSpec
import qualified Saturn.Unstable.Type.Field as Field
import qualified Saturn.Unstable.Type.Wildcard as Wildcard
import qualified Saturn.Unstable.Type.WildcardSpec as WildcardSpec
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Heck.Test IO n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Type.Field" $ do
  Heck.it t "round trips"
    . QuickCheck.quickCheck
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Field.parsec "" (Builder.toLazyText $ Field.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Field.Field
arbitrary =
  Field.fromEither
    <$> QuickCheck.liftArbitrary2
      WildcardSpec.arbitrary
      (arbitraryNonEmpty ElementSpec.arbitrary)

arbitraryNonEmpty :: QuickCheck.Gen a -> QuickCheck.Gen (NonEmpty.NonEmpty a)
arbitraryNonEmpty g = (NonEmpty.:|) <$> g <*> QuickCheck.listOf g

shrink :: Field.Field -> [Field.Field]
shrink field =
  let xs = case Field.toEither field of
        Left _ -> []
        Right _ -> [Field.fromEither . Left $ Wildcard.fromUnit ()]
   in mappend xs
        . fmap Field.fromEither
        . QuickCheck.liftShrink2 WildcardSpec.shrink (shrinkNonEmpty ElementSpec.shrink)
        $ Field.toEither field

shrinkNonEmpty :: (a -> [a]) -> NonEmpty.NonEmpty a -> [NonEmpty.NonEmpty a]
shrinkNonEmpty f = Maybe.mapMaybe (NonEmpty.nonEmpty . f) . NonEmpty.toList

new :: (MonadFail m) => [[Word.Word8]] -> m Field.Field
new =
  fmap
    ( Field.fromEither
        . maybe (Left $ Wildcard.fromUnit ()) Right
        . NonEmpty.nonEmpty
    )
    . mapM ElementSpec.new
