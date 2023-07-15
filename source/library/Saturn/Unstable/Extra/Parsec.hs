module Saturn.Unstable.Extra.Parsec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Parsec as Parsec

either ::
  Parsec.ParsecT s u m a ->
  Parsec.ParsecT s u m b ->
  Parsec.ParsecT s u m (Either a b)
either l r = fmap Left l Parsec.<|> fmap Right r

sepByNE ::
  Parsec.ParsecT s u m a ->
  Parsec.ParsecT s u m sep ->
  Parsec.ParsecT s u m (NonEmpty.NonEmpty a)
sepByNE p s = (NonEmpty.:|) <$> p <*> Parsec.many (s *> p)
