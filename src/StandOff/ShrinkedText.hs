{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
module StandOff.ShrinkedText
  ( OffsetMapping
  , InflatableMarkup(..)
  , shrinkedText
  , shrinkedText'
  ) where

import Data.Tree.Class
import Data.Monoid

import StandOff.StringLike (StringLike)
import StandOff.XTraverse




-- | A mapping of offsets in the XML file to offsets in the generated
-- plain text file. The list index represents the XML offset, the
-- element value the offset in the plain text file.
type OffsetMapping = [(Int, Int)]

-- data OffsetMapping = OffsetMapping
--   { offsetMap_offsetMapping :: [Int] -- ^ mapping of character offsets
--   , offsetMap_byteMapping :: [Int]   -- ^ mapping of byte offsets
--   }

initialOffsetMapping :: OffsetMapping
-- initialOffsetMapping = OffsetMapping [] []
initialOffsetMapping = []


-- | 'InflatableMarkup' is an interface for annotations, the positions
-- of which must be mapped to positions in the source test using an
-- 'OffsetMapping'.
class InflatableMarkup a where
  -- | 'inflate' applies an offset mapping to a markup element
  -- (annotation) and maps the position in the shrinked text to the
  -- position in the source text. If the positions in a exceeds the
  -- domain of the mapping, 'Left' is returned.
  inflate :: OffsetMapping -> a -> Either String a


-- * Generate shrinked text

-- | Generate shrinked text.
--
-- Note, that the equidistant nodes, the serialized representation of
-- the source document, and the replacement strings in the shrinking
-- node configuration share the same string-like type. Also note, that
-- the shrinking nodes and the configuration share the same type for
-- node names.
shrinkedText
  :: (StringLike s, Monoid s, Monad m, Tree t) =>
     (s -> m ())  -- ^ monadic writer for shrinked text
  -> (n -> (s, OffsetMapping) -> (s, (s, OffsetMapping))) -- ^ function that replaces an open node with a string like
  -> (n -> (s, OffsetMapping) -> (s, (s, OffsetMapping))) -- ^ function that replaces a close node with a string like
  -> [t n]        -- ^ the parsed XML document
  -> s            -- ^ the XML document as a string
  -> m (OffsetMapping) -- ^ the offset mapping is returned
shrinkedText writeM funOpen funClose xml s = do
  final <- xtraverseWithState writeM funOpen funClose xml (s, initialOffsetMapping)
  -- We cannot write what is left over, because we do not have a
  -- mapping for it. Should we do a test on the final state or write
  -- what is left over?
  --
  -- writeM $ fst final
  return $ snd final

shrinkedText'
  :: (StringLike s, Monoid s, Monad m, Tree t) =>
     (s -> m ())  -- ^ monadic writer for shrinked text
  -> (n -> s -> (s, -- ^ string to write
                 s, -- ^ new doc string to be read
                 OffsetMapping -- ^ new part of offset mapping
                ))             -- ^ a function to be applied on open tags
     -- ^ function that replaces an open node with a string like
  -> (n -> s -> (s, s, OffsetMapping)) -- ^ function that replaces a close node with a string like
  -> [t n]        -- ^ the parsed XML document
  -> s            -- ^ the XML document as a string
  -> m (OffsetMapping) -- ^ the offset mapping is returned
shrinkedText' writeM f g xml doc = shrinkedText writeM (toShrinkingStateFun f) (toShrinkingStateFun g) xml doc


toShrinkingStateFun :: (n -> s -> (s, s, OffsetMapping)) -> n -> (s, OffsetMapping) -> (s, (s, OffsetMapping))
toShrinkingStateFun f node (doc, offsets) = (outString, (newDoc, offsets <> outMapping))
  where
    (outString, newDoc, outMapping) = f node doc
