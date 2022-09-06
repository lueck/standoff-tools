{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
module StandOff.ShrinkedText
  ( OffsetMapping
  , InflatableMarkup(..)
  , ShrinkingNode(..)
  , ShrinkingNodeConfig(..)
  , shrinkedText
  ) where

import Control.Monad.State
import Data.Tree.Class

import StandOff.StringLike (StringLike)
import StandOff.XTraverse
import StandOff.Utils


-- * Classes

-- Having ShrinkingNode and ShrinkingNodeConfig both as classes does
-- not work. This does not work, because the compiler wants the config
-- for all node which are a ShrinkingNode! There is no way to tie a
-- specific config to a specific node type.


-- | The class 'ShrinkingNode' for markup the nodes of which are
-- shrinkable.
--
-- A shrinking node is expected to be parametrized with two types. The
-- first type parameter k is the type of the node names, e.g. 'QName'
-- for XML. This type is also used in the configuration for shrinking
-- nodes. ItThis type has to be an instance of 'Eq' and 'Ord'. The second type
-- parameter s is the type of the text nodes of the markup. This type
-- ist also used in the configuration for shrinking nodes and the
-- source document must be given as this type.
class (StringLike s, Ord k) => ShrinkingNode n k s where
  -- | This processes an opening part of a node (in case of an element
  -- node) or the complete node
  shrinkOpen
    :: (ShrinkingNodeConfig c) =>
       c                       -- ^ a configuration
    -> n k s                   -- ^ the node
    -> s                       -- ^ the input string from this node till EOF
    -> OffsetMapping           -- ^ the the offset mapping generated so far
    -> (s, (s, OffsetMapping)) -- ^ returns a tupel of output string
                               -- and new state of left input string
                               -- and new offset mapping

  -- | This processes the closing part of a node. The arguments are
  -- the same as in 'serializeOpen'.
  shrinkClose
    :: (ShrinkingNodeConfig c) =>
       c
    -> n k s
    -> s
    -> OffsetMapping
    -> (s, (s, OffsetMapping))

-- | A configuration for shrinked text is parametrized with a type for
-- node names and a string like type for the replacement text.
class ShrinkingNodeConfig c where
  -- | replace open tag (or the whole node) with a string like text.
  replaceOpen
    :: (Ord k, StringLike s, ShrinkingNode n k s) =>
       c     -- ^ the config
    -> n k s -- ^ the node
    -> s     -- ^ the string like replacement

  -- | replace close tag with a string like text. In case of nodes
  -- that have no close tag, the result should be empty string. The
  -- arguments are the same as in 'replaceOpen'.
  replaceClose :: (ShrinkingNode n k s) => c -> n k s -> s


-- | A mapping of offsets in the XML file to offsets in the generated
-- plain text file. The list index represents the XML offset, the
-- element value the offset in the plain text file.
type OffsetMapping = [Int]

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
  :: (StringLike s, Monoid s, Monad m, Tree t, ShrinkingNode n k s, Ord k, ShrinkingNodeConfig c) =>
     (s -> m ())  -- ^ monadic writer for shrinked text
  -> c            -- ^ the configuration
  -> [t (n k s)]  -- ^ the parsed XML document
  -> s            -- ^ the XML document as a string
  -> m (OffsetMapping) -- ^ the offset mapping is returned
shrinkedText writeM nodeCfg xml s = do
  final <- xtraverseWithState writeM f g xml (s, initialOffsetMapping)
  -- We cannot write what is left over, because we do not have a
  -- mapping for it. Should we do a test on the final state or write
  -- what is left over?
  --
  -- writeM $ fst final
  return $ snd final
  where
    f n = uncurry (shrinkOpen nodeCfg n)
    g n = uncurry (shrinkClose nodeCfg n)
