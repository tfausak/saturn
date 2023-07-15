module Saturn.Unstable.Extra.TupleSpec where

import qualified Saturn.Unstable.Extra.Tuple as Tuple
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Saturn.Unstable.Extra.Tuple" $ do
  Hspec.describe "mapBoth" $ do
    Hspec.it "works" $ do
      Tuple.mapBoth succ ('A', 'a') `Hspec.shouldBe` ('B', 'b')

  Hspec.describe "toSequence" $ do
    Hspec.it "works" $ do
      Tuple.toSequence ('b', 'd') `Hspec.shouldBe` "bcd"

    Hspec.it "works with singleton" $ do
      Tuple.toSequence ('a', 'a') `Hspec.shouldBe` "a"

    Hspec.it "works with empty" $ do
      Tuple.toSequence ('b', 'a') `Hspec.shouldBe` ""
