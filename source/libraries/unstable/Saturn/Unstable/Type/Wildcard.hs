{-# LANGUAGE FlexibleContexts #-}

module Saturn.Unstable.Type.Wildcard where

import qualified Control.Monad as Monad
import qualified Data.Coerce as Coerce
import qualified Data.Text.Lazy.Builder as Builder
import qualified Text.Parsec as Parsec

newtype Wildcard
  = Wildcard ()
  deriving (Eq, Show)

fromUnit :: () -> Wildcard
fromUnit = Coerce.coerce

toUnit :: Wildcard -> ()
toUnit = Coerce.coerce

parsec :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Wildcard
parsec = fmap fromUnit . Monad.void $ Parsec.char '*'

toBuilder :: Wildcard -> Builder.Builder
toBuilder = const $ Builder.singleton '*'
