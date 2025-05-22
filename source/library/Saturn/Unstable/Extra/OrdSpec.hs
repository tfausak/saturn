module Saturn.Unstable.Extra.OrdSpec where

import qualified Heck
import qualified Saturn.Unstable.Extra.Ord as Ord

spec :: (Applicative m, Monad n) => Heck.Test m n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Extra.Ord" $ do
  Heck.describe t "within" $ do
    Heck.it t "fails below lower bound" $ do
      Heck.assertEq t False (Ord.within ('b', 'd') 'a')

    Heck.it t "succeeds at lower bound" $ do
      Heck.assertEq t True (Ord.within ('b', 'd') 'b')

    Heck.it t "succeeds within bounds" $ do
      Heck.assertEq t True (Ord.within ('b', 'd') 'c')

    Heck.it t "succeeds at upper bound" $ do
      Heck.assertEq t True (Ord.within ('b', 'd') 'd')

    Heck.it t "fails above upper bound" $ do
      Heck.assertEq t False (Ord.within ('b', 'd') 'e')
