{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
module StandOff.ShrinkedText
where

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Tree.Class
import qualified Data.YAML as Y
import Data.YAML ((.:), (.!=), (.:?))
import qualified Data.Aeson as J
import GHC.Generics
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import StandOff.LineOffsets
import StandOff.StringLike (StringLike)
import qualified StandOff.StringLike as SL
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
data ShrinkingNodeConfig k s = ShrinkingNodeConfig
  { _shrinkCfg_tagReplacements :: (ShrinkingNodeReplacements k s)
  , _shrinkCfg_defaultTagReplacement :: ShrinkingNodeReplacement s
  , _shrinkCfg_defaultPiReplacement :: s
  -- replacing entities should be done by the parser!?
  , _shrinkCfg_entityReplacements :: EntityResolver s
  , _shrinkCfg_defaultEntityReplacements :: s
  }
  deriving (Show, Generic)

-- | A config of nodes to replacement strings. There are type
-- parameters for the nodes' names (@k@) and the replacement strings
-- (@s@).
type ShrinkingNodeReplacements k s = Map k (ShrinkingNodeReplacement s)

data ShrinkingNodeReplacement s = ShrinkingNodeReplacement
  { _shrinkRepl_open :: s
  , _shrinkRepl_close :: s
  , _shrinkRepl_empty :: s
  }
  deriving (Show, Generic)

-- | A mapping of entity names to resolved strings.
type EntityResolver s = Map String s


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
    :: ShrinkingNodeConfig k s -- ^ a configuration
    -> n k s                   -- ^ the node
    -> s                       -- ^ the input string from this node till EOF
    -> OffsetMapping           -- ^ the the offset mapping generated so far
    -> (s, (s, OffsetMapping)) -- ^ returns a tupel of output string
                               -- and new state of left input string
                               -- and new offset mapping

  -- | This processes the closing part of a node. The arguments are
  -- the same as in 'serializeOpen'.
  shrinkClose
    :: ShrinkingNodeConfig k s
    -> n k s
    -> s
    -> OffsetMapping
    -> (s, (s, OffsetMapping))


-- | Generate shrinked text.
--
-- Note, that the equidistant nodes, the serialized representation of
-- the source document, and the replacement strings in the shrinking
-- node configuration share the same string-like type. Also note, that
-- the shrinking nodes and the configuration share the same type for
-- node names.
shrinkedText
  :: (StringLike s, Monoid s, Monad m, Tree t, ShrinkingNode n k s, Ord k) =>
     (s -> m ())  -- ^ monadic writer for shrinked text
  -> ShrinkingNodeConfig k s  -- ^ the configuration
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


-- * Parsing the config from yaml

defaultShrinkingConfig :: StringLike s => ShrinkingNodeConfig k s
defaultShrinkingConfig = ShrinkingNodeConfig Map.empty defaultShrinkingNodeReplacement SL.empty Map.empty SL.empty

defaultShrinkingNodeReplacement :: StringLike s => ShrinkingNodeReplacement s
defaultShrinkingNodeReplacement = ShrinkingNodeReplacement SL.empty SL.empty SL.empty


-- Note: There is no instance FromYAML String!
instance (StringLike s, Y.FromYAML s) => Y.FromYAML (ShrinkingNodeConfig T.Text s) where
  parseYAML = Y.withMap "shrink" $ \m -> ShrinkingNodeConfig
    <$> m .:? "tags" .!= Map.empty
    <*> m .:? "tagDefault" .!= defaultShrinkingNodeReplacement
    <*> m .:? "piDefault" .!= SL.empty
    <*> (fmap (Map.mapKeys T.unpack) $ m .:? "entities" .!= Map.empty)
    <*> m .:? "entityDefault" .!= SL.empty

instance (StringLike s, Y.FromYAML s) => Y.FromYAML (ShrinkingNodeReplacement s) where
  parseYAML = Y.withMap "element" $ \m -> ShrinkingNodeReplacement
    <$> m .:? "open" .!= SL.empty
    <*> m .:? "close" .!= SL.empty
    <*> m .:? "empty" .!= SL.empty

-- | USAGE:
--
-- > do { BL.readFile "mappings/shrink-tei.yaml" >>= parseShrinkingConfig }
parseShrinkingConfig :: BL.ByteString -> IO (ShrinkingNodeConfig T.Text T.Text)
parseShrinkingConfig c = do
  case Y.decode c of
    Left (pos, err) -> do
      fail $ show pos ++ " " ++ show err
    Right cfg -> do
      return $ head cfg
