module StandOff.Tag
  ( TagType(..)
  , TagSerializer
  , constTagSerializer
  , featureTagSerializer
  )
where

-- | This module provides types and functions for serializing tags.

import StandOff.External
import StandOff.AttributesMap
import Data.Monoid
import qualified Data.Map as Map
import Data.Maybe

-- | Tag types for use in 'internalize'.
data TagType
  = Open
  | Close
  | Empty
  deriving (Eq, Show)


-- | A tag serializer takes a 'TagType' and a tag and returns the tag in
-- serialized form.
type TagSerializer a = (TagType -> a -> String)


-- | A serializer that produces tags with the same name. The name must
-- be given in the first parameter.
constTagSerializer
  :: (ToAttributes a, IdentifiableSplit a) =>
     String                              -- ^ name of element
  -> (ExternalAttributes -> [Attribute]) -- ^ a partially evaluated attributes mapping
  -> TagSerializer a                     -- ^ returns a 'TagSerializer'
constTagSerializer elName mapping Open tag =
  "<" <> elName
  <> (concatMap ((" "<>) . serializeXml) $ mapping $ specialAttrs tag $ attributes tag)
  <> ">"
constTagSerializer elName mapping Empty tag =
  "<" <> elName
  <> (concatMap ((" "<>) . serializeXml) $ mapping $ specialAttrs tag $ attributes tag)
  <> "/>"
constTagSerializer elName _ Close _ =
  "</" <> elName <> ">"


-- | A serializer that produces tags with the tag name taken from a
-- feature of the annotation. If the feature is not present, the
-- feature name will be taken instead.
featureTagSerializer
  :: (ToAttributes a, IdentifiableSplit a) =>
     String                              -- ^ name of feature to make tag name from
  -> String                              -- ^ fallback tag name in case feature not present
  -> (ExternalAttributes -> [Attribute]) -- ^ a partially evaluated attributes mapping
  -> TagSerializer a                     -- ^ returns a 'TagSerializer'
featureTagSerializer feature fallback mapping Open tag =
  "<" <> (tagNameFromFeature feature fallback tag)
  <> (concatMap ((" "<>) . serializeXml) $ mapping $ specialAttrs tag $ attributes tag)
  <> ">"
featureTagSerializer feature fallback mapping Empty tag =
  "<" <> (tagNameFromFeature feature fallback tag)
  <> (concatMap ((" "<>) . serializeXml) $ mapping $ specialAttrs tag $ attributes tag)
  <> "/>"
featureTagSerializer feature fallback _ Close tag =
  "</" <> (tagNameFromFeature feature fallback  tag) <> ">"


tagNameFromFeature :: (ToAttributes a) => String -> String -> a -> String
tagNameFromFeature feature fallback tag = fromMaybe fallback $ Map.lookup feature $ attributes tag
