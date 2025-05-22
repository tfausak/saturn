module Saturn.Unstable.Type.WildcardSpec where

import qualified Data.Text.Lazy.Builder as Builder
import qualified Saturn.Unstable.Type.Wildcard as Wildcard
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Type.Wildcard" $ do
  Hspec.it "round trips"
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Wildcard.parsec "" (Builder.toLazyText $ Wildcard.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Wildcard.Wildcard
arbitrary = Wildcard.fromUnit <$> QuickCheck.arbitrary

shrink :: Wildcard.Wildcard -> [Wildcard.Wildcard]
shrink = fmap Wildcard.fromUnit . QuickCheck.shrink . Wildcard.toUnit
