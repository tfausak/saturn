module Saturn.Unstable.Type.WildcardSpec where

import qualified Data.Text.Lazy.Builder as Builder
import qualified Heck
import qualified Saturn.Unstable.Type.Wildcard as Wildcard
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Heck.Test IO n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Type.Wildcard" $ do
  Heck.it t "round trips"
    . QuickCheck.quickCheck
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Wildcard.parsec "" (Builder.toLazyText $ Wildcard.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Wildcard.Wildcard
arbitrary = Wildcard.fromUnit <$> QuickCheck.arbitrary

shrink :: Wildcard.Wildcard -> [Wildcard.Wildcard]
shrink = fmap Wildcard.fromUnit . QuickCheck.shrink . Wildcard.toUnit
