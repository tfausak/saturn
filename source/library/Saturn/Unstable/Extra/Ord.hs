module Saturn.Unstable.Extra.Ord where

within :: (Ord a) => (a, a) -> a -> Bool
within (lo, hi) x = lo <= x && x <= hi
