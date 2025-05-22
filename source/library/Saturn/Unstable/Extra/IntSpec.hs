module Saturn.Unstable.Extra.IntSpec where

import qualified Heck
import qualified Saturn.Unstable.Extra.Int as Int

spec :: (Applicative m, Monad n) => Heck.Test m n -> n ()
spec t = Heck.describe t "Saturn.Unstable.Extra.Int" $ do
  Heck.describe t "fromWord8" $ do
    Heck.it t "works" $ do
      Heck.assertEq t (Int.fromWord8 0) 0

  Heck.describe t "toWord8" $ do
    Heck.it t "succeeds with minimum" $ do
      Heck.assertEq t (Int.toWord8 0) (Just 0)

    Heck.it t "succeeds with maximum" $ do
      Heck.assertEq t (Int.toWord8 255) (Just 255)

    Heck.it t "fails below minimum" $ do
      Heck.assertEq t (Int.toWord8 (-1)) Nothing

    Heck.it t "fails above maximum" $ do
      Heck.assertEq t (Int.toWord8 256) Nothing
