{-# LANGUAGE DeriveGeneric #-}
module StandOff.AttributesMap
  ( AttributesMap
  , AttributeMap(..)
  , ValueMap
  , Attribute(..)
  , mapExternal
  , serializeXml
  , specialAttrs
  , parseMapping
  )
where

-- | This module provides types and functions for mapping features
-- (attributes) of external markup to XML attributes.  The aim is to
-- plug in an arbitrary mapping definition for serializing to
-- arbitrary attributes.
--
-- No processing is done on the attribute values.  The features come
-- from 'attributes' of markup that is an instance of 'ToAttributes'.
-- Further processing of attribute values can be done with
-- X-technologies in postprocessing.
--
-- There are also special attributes produced by this module.  See
-- 'specialAttrs'.

import qualified Data.Map as Map
import Data.Map ((!))
import Data.Maybe
import Control.Monad
import Data.Monoid
import qualified Data.Yaml as Y
import qualified Data.Aeson as J
import GHC.Generics

import StandOff.External


-- * Types for mapping features of external markup to XML attributes

-- | An mapping of attributes provided by external markup that
-- implements the class 'ToAttributes'. The mapping is done by the
-- keys of both 'Map.Map's.
type AttributesMap = Map.Map String AttributeMap

-- | A record holding full qualified names etc and a 'ValueMap'.
data AttributeMap = AttributeMap
  { attrm_prefix :: Maybe String
  , attrm_ns :: Maybe String
  , attrm_name :: String
  , attrm_values :: Maybe ValueMap
  } deriving (Eq, Show, Generic)

-- | Mapping of values of external markup to some other values.
type ValueMap = Map.Map String String

-- | An attribute that was mapped from the external markup's
-- attributes.
data Attribute = Attribute
  { attr_prefix :: Maybe String
  , attr_ns :: Maybe String
  , attr_name :: String
  , attr_value :: String
  } deriving (Eq, Show)


-- * Parser for mappings in YAML

instance Y.FromJSON AttributeMap where
  parseJSON = J.genericParseJSON $ J.defaultOptions { J.fieldLabelModifier = drop 6 }


-- | Parse 'AttributesMap' from file.
parseMapping :: FilePath -> IO (Either Y.ParseException AttributesMap)
parseMapping = Y.decodeFileEither


-- | Debugging: USAGE:
-- stack runghc src/StandOff/AttributesMap.hs
main :: IO ()
main = parseMapping "utils/mappings/som-tei.yaml" >>= print


-- * Do the mapping

-- | 'mapExternal' does the work of mapping attributes of external
-- markup through an 'AttributesMap'.
mapExternal :: AttributesMap -> ExternalAttributes -> [Attribute]
mapExternal mapping external =
  map (\(k, v) -> toAttr (mapping ! k) v) $
  Map.toList $
  Map.restrictKeys external keys
  where
    keys = Map.keysSet mapping
    toAttr m v =
      Attribute
      { attr_prefix = attrm_prefix m
      , attr_ns = attrm_ns m
      , attr_name = attrm_name m
      , attr_value = fromMaybe v $ join $ fmap (Map.lookup v) $ attrm_values m
      }

-- | Serialize an 'Attribute' to XML.
serializeXml :: Attribute -> String
serializeXml attr = attr_name attr <> "=\"" <> (escape $ attr_value attr) <> "\""
  where
    escape = id -- TODO: escape quotes


-- * Generating special features or attributes

-- | Update a map of external attributes with special attributes
-- generated by standoff-tools. E.g. this adds compound identifiers
-- from the markup identifier and the split number, which can be used
-- in TEI's @prev attribute for discontinous markup.
specialAttrs :: IdentifiableSplit a => a -> ExternalAttributes -> ExternalAttributes
specialAttrs tag =
  appendPrevId tag .
  appendSplitId tag

-- | This adds the special attribute "__standoff_special__splitId"
-- which can be used as @xml:id.
appendSplitId :: IdentifiableSplit a => a -> ExternalAttributes -> ExternalAttributes
appendSplitId tag attrs
  = Map.insert "__standoff_special__splitId" (splitIdValue (markupId tag) (splitNum tag)) attrs

-- | This adds the special attribute "__standoff_special__prevId"
-- which can be used as TEI's @prev.
appendPrevId :: IdentifiableSplit a => a -> ExternalAttributes -> ExternalAttributes
appendPrevId tag attrs
  | fmap (>0) (splitNum tag) == (Just True)
  = Map.insert "__standoff_special__prevId" ("#" <> splitIdValue (markupId tag) (fmap (\x -> x - 1) $ splitNum tag)) attrs
  | otherwise = attrs

splitIdValue :: Maybe String -> Maybe Int -> String
splitIdValue Nothing _ = "unknown"
splitIdValue (Just mid) Nothing = mid
splitIdValue (Just mid) (Just 0) = mid
splitIdValue (Just mid) (Just i) =  mid <> "-" <> (show i)
