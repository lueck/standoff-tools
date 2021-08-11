module StandOff.AttributesMap
where

import qualified Data.Map as Map
import Data.Map ((!))
import Data.Maybe
import Control.Monad
import Data.Monoid

import StandOff.External

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
  } deriving (Eq, Show)

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
