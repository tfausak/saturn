module Saturn.Unstable.Type.NumberSpec where

import qualified Data.Text.Lazy.Builder as Builder
import qualified Saturn.Unstable.Type.Number as Number
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Text.Parsec as Parsec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Type.Number" $ do
  Hspec.it "round trips"
    . QuickCheck.forAllShrink arbitrary shrink
    $ \x -> do
      Parsec.parse Number.parsec "" (Builder.toLazyText $ Number.toBuilder x)
        `Hspec.shouldBe` Right x

arbitrary :: QuickCheck.Gen Number.Number
arbitrary = Number.fromWord8 <$> QuickCheck.arbitrary

shrink :: Number.Number -> [Number.Number]
shrink = fmap Number.fromWord8 . QuickCheck.shrink . Number.toWord8
