{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TupleSections #-}
module StandOff.ShrinkedText
where

import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Tree.Class

import StandOff.LineOffsets
import StandOff.StringLike (StringLike)
import StandOff.XTraverse


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

-- | A record for the configuration that determines shrinking of
-- all kinds of nodes.
data (StringLike s) => ShrinkingNodeConfig s = ShrinkingNodeConfig
  { _shrinkCfg_tagReplacements :: (ShrinkingNodeReplacements s)
  , _shrinkCfg_defaultTagReplacement :: ShrinkingNodeReplacement s
  , _shrinkCfg_defaultPiReplacement :: s
  -- replacing entities should be done by the parser!?
  , _shrinkCfg_entityReplacements :: EntityResolver s
  , _shrinkCfg_defaultEntityReplacements :: s
  }

-- | A config of node qnames to replacement strings.
type ShrinkingNodeReplacements s = Map String (ShrinkingNodeReplacement s)
-- TODO: replace String key type with QName

data (StringLike s) => ShrinkingNodeReplacement s = ShrinkingNodeReplacement
  { _shrinkRepl_open :: s
  , _shrinkRepl_close :: s
  , _shrinkRepl_empty :: s
  }

-- | A mapping of entity names to resolved strings.
type EntityResolver s = Map String s

class ShrinkingNode n where
  -- | This processes an opening part of a node (in case of an element
  -- node) or the complete node
  shrinkOpen
    :: (StringLike s) =>
       ShrinkingNodeConfig s -- ^ a configuration
    -> n                -- ^ the node
    -> s                -- ^ the input string from this node till EOF
    -> OffsetMapping    -- ^ the the offset mapping generated so far
    -> (s, (s, OffsetMapping)) -- ^ returns a tupel of output string
                               -- and new state of left input string
                               -- and new offset mapping

  -- | This processes the closing part of a node. The arguments are
  -- the same as in 'serializeOpen'.
  shrinkClose
    :: (StringLike s) =>
       ShrinkingNodeConfig s
    -> n
    -> s
    -> OffsetMapping
    -> (s, (s, OffsetMapping))


-- | Generate shrinked text.
shrinkedText'
  :: (StringLike s, Monoid s, Monad m, Tree t, ShrinkingNode n) =>
     (s -> m ())  -- ^ monadic action
  -> ShrinkingNodeConfig s
  -> [t n]        -- ^ the parsed XML document
  -> s            -- ^ the XML document as a string
  -> m (OffsetMapping)
shrinkedText' writeM nodeCfg xml s = do
  final <- execStateT (mapM_ (xtraverse_ writeM f g) xml) (s, initialOffsetMapping)
  -- We cannot write what is left over, because we do not have a
  -- mapping for it. Should we do a test on the final state or write
  -- what is left over?
  --
  -- writeM $ fst final
  return $ snd final
  where
    f n = uncurry (shrinkOpen nodeCfg n)
    g n = uncurry (shrinkClose nodeCfg n)


shrinkedText
  :: (StringLike s, Monoid s, Monad m, Tree t, ShrinkingNode n) =>
     (s -> m ())  -- ^ monadic action
  -> ShrinkingNodeConfig s
  -> [t n]        -- ^ the parsed XML document
  -> s            -- ^ the XML document as a string
  -> m (OffsetMapping)
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


