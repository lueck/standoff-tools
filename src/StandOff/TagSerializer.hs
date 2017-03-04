module StandOff.TagSerializer where

import Data.List

import StandOff.AnnotationTypeDefs
import StandOff.TagTypeDefs

-- Split http://arb.org/schema/Concept or
-- http://arb.org/schema#Concept to a tuple of namespace and name,
-- here ("http://arb.org/schema/", "Concept") or
-- ("http://arb.org/schema#", "Concept").
splitNamespaceName :: String -> (String, String)
splitNamespaceName name = splitAt splitPos name
  where splitPos = (max
                    (maximum $ 0 : elemIndices '/' name)
                    (maximum $ 0 : elemIndices '#' name)) + 1

nsNameValueSerializer :: NSNameValueSerializer -> String -> String
nsNameValueSerializer FullName name = name
nsNameValueSerializer LocalName name = snd $ splitNamespaceName name

-- Simple serializer for an XML tag. Suitable only, if range types and
-- relation predicates don't have a namespace.
serializeTag :: (Annotation -> String) -> TagType -> Annotation -> String
serializeTag slizer Open a
  = "<" ++ (rangeType a) ++ (slizer a) ++ ">"
serializeTag slizer Close a
  = "</" ++ (rangeType a) ++ ">"
serializeTag slizer Empty a
  = "<" ++ (rangeType a) ++ (slizer a) ++ "/>"

-- A serializer that creates tags always with the same tag-name. This
-- tag name is passed as a parameter. The serialization of the type
-- and range-id and element-id are left to attribute-serializer which
-- is passed as a curried function.
serializeSpanTag :: (Annotation -> String) -> String -> TagType -> Annotation -> String
serializeSpanTag slizeAttrs tagName Open a
  = "<" ++ tagName
    ++ slizeAttrs a
    ++ ">"
serializeSpanTag _ tagName Close _
  = "</" ++ tagName ++ ">"
serializeSpanTag slizeAttrs tagName Empty a
  = "<" ++ tagName
    ++ slizeAttrs a
    ++ "/>"

serializeNsTag :: (Annotation -> String) -> String -> TagType -> Annotation -> String
serializeNsTag slizeAttrs prefix Open a =
  "<" ++ prefix ++ ":" ++ (snd qName)
  ++ " xmlns:" ++ prefix ++ "=\"" ++ (fst qName) ++ "\""
  ++ slizeAttrs a
  ++ ">"
  where qName = splitNamespaceName $ rangeType a
serializeNsTag _ prefix Close a =
  "</" ++ prefix ++ ":" ++ (snd $ splitNamespaceName $ rangeType a) ++ ">"
serializeNsTag slizeAttrs prefix Empty a =
  "<" ++ prefix ++ ":" ++ (snd qName)
  ++ " xmlns:" ++ prefix ++ "=\"" ++ (fst qName) ++ "\""
  ++ slizeAttrs a
  ++ "/>"
  where qName = splitNamespaceName $ rangeType a
