module Saturn.Unstable.Extra.Tuple where

import qualified Data.Bifunctor as Bifunctor

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f = Bifunctor.bimap f f

toSequence :: (Enum a) => (a, a) -> [a]
toSequence = uncurry enumFromTo
