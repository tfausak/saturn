module Saturn.Unstable.Extra.OrdSpec where

import qualified Saturn.Unstable.Extra.Ord as Ord
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Extra.Ord" $ do
  Hspec.describe "within" $ do
    Hspec.it "fails below lower bound" $ do
      'a' `Hspec.shouldNotSatisfy` Ord.within ('b', 'd')

    Hspec.it "succeeds at lower bound" $ do
      'b' `Hspec.shouldSatisfy` Ord.within ('b', 'd')

    Hspec.it "succeeds within bounds" $ do
      'c' `Hspec.shouldSatisfy` Ord.within ('b', 'd')

    Hspec.it "succeeds at upper bound" $ do
      'd' `Hspec.shouldSatisfy` Ord.within ('b', 'd')

    Hspec.it "fails above upper bound" $ do
      'e' `Hspec.shouldNotSatisfy` Ord.within ('b', 'd')
