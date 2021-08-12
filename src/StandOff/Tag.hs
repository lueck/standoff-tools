module StandOff.Tag
  ( TagType(..)
  , TagSerializer
  , constTagSerializer
  )
where

-- | This module provides types and functions for serializing tags.

import StandOff.External
import StandOff.AttributesMap
import Data.Monoid


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
constTagSerializer :: (ToAttributes a, IdentifiableSplit a) =>
                      String -> (ExternalAttributes -> [Attribute]) -> TagSerializer a
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
