{-# LANGUAGE FlexibleContexts #-}

-- | This module provides a parser that produces a mapping of parsec's
-- 'SourcePos' to character offsets represented by 'Int'. With that
-- mapping it is possible to map parsec's position to integers no
-- matter of parsec's special handling of the tab character.

module StandOff.OffsetMapping
  ( OffsetMapping
  , parsecOffsetMapping
  ) where

import Text.Parsec
import Data.Functor.Identity (Identity)


type OffsetMapping = [(Int, SourcePos)]

offset :: Stream s Identity Char => Parsec s () SourcePos
offset = do
  pos <- getPosition
  _ <- anyChar
  return pos

parsecOffsetMapping
  :: (Monad m, Stream s Identity Char) =>
     Int    -- ^ how the offsets are to be indexed, e.g. 0 for zero indexed
  -> String -- ^ the systemId of the input stream, used for error messages
  -> s      -- ^ the input stream
  -> m OffsetMapping
parsecOffsetMapping indexed location contents = do
  return $ either (fail . (err++) . show) (zip [indexed ..]) (parse (many offset) location contents)
  where
    err = "Error parsing offset mapping (" ++ location ++ ") : "
