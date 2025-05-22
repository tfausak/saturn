module Saturn.Unstable.Extra.TupleSpec where

import qualified Heck
import qualified Saturn.Unstable.Extra.Tuple as Tuple

spec :: (Applicative m, Monad n) => Heck.Test m n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Extra.Tuple" $ do
  Heck.describe t "mapBoth" $ do
    Heck.it t "works" $ do
      Heck.assertEq t ('B', 'b') (Tuple.mapBoth succ ('A', 'a'))

  Heck.describe t "toSequence" $ do
    Heck.it t "works" $ do
      Heck.assertEq t "bcd" (Tuple.toSequence ('b', 'd'))

    Heck.it t "works with singleton" $ do
      Heck.assertEq t "a" (Tuple.toSequence ('a', 'a'))

    Heck.it t "works with empty" $ do
      Heck.assertEq t "" (Tuple.toSequence ('b', 'a'))
