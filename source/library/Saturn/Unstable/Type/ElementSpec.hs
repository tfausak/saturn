module Saturn.Unstable.Type.ElementSpec where

import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Word as Word
import qualified Saturn.Unstable.Type.Element as Element
import qualified Saturn.Unstable.Type.Number as Number
import qualified Saturn.Unstable.Type.NumberSpec as NumberSpec
import qualified Saturn.Unstable.Type.Range as Range
import qualified Saturn.Unstable.Type.RangeSpec as RangeSpec
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Type.Element" $ do
  Hspec.it "round trips"
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Element.parsec "" (Builder.toLazyText $ Element.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Element.Element
arbitrary =
  Element.fromEither
    <$> QuickCheck.liftArbitrary2
      RangeSpec.arbitrary
      NumberSpec.arbitrary

shrink :: Element.Element -> [Element.Element]
shrink element =
  let xs = case Element.toEither element of
        Left range ->
          let (lo, hi) = Range.toTuple range
           in fmap (Element.fromEither . Right) [lo, hi]
        Right _ -> []
   in mappend xs
        . fmap Element.fromEither
        . QuickCheck.liftShrink2 RangeSpec.shrink NumberSpec.shrink
        $ Element.toEither element

new :: (MonadFail m) => [Word.Word8] -> m Element.Element
new xs = case xs of
  [x] -> pure . Element.fromEither . Right $ Number.fromWord8 x
  [x, y] -> Element.fromEither . Left <$> RangeSpec.new (x, y)
  _ -> fail $ "invalid Element: " <> show xs
