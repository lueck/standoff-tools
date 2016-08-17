module StandOff.XML.TagSerializer where

import Data.List
import qualified Data.Map as Map
import Data.UUID (toString)

import StandOff.Data.Annotation
import StandOff.Data.Tag

serializeAttributes :: Annotation -> String
serializeAttributes a =
  " elementId=\"" ++ (toString (rangeElementId a)) ++ "\""
  ++ " rangeId=\"" ++ (emptyStringWhenNothing (fmap toString (rangeRangeId a))) ++ "\""
  ++ (foldl (\acc (k, v) -> acc ++ " " ++ k ++ "=\"" ++ (intercalate " " v) ++ "\"")
       ""
       (Map.toList $ rangeAttributes a))
  where emptyStringWhenNothing Nothing = ""
        emptyStringWhenNothing (Just s) = s

-- Split http://arb.org/schema/Concept or
-- http://arb.org/schema#Concept to a tuple of namespace and name,
-- here ("http://arb.org/schema/", "Concept") or
-- ("http://arb.org/schema#", "Concept").
splitNamespaceName :: Annotation -> (String, String)
splitNamespaceName a = splitAt splitPos typ
  where typ = rangeType a
        splitPos = (max
                    (maximum $ 0 : elemIndices '/' typ)
                    (maximum $ 0 : elemIndices '#' typ)) + 1

-- Simple serializer for an XML tag. Suitable only, if range types and
-- relation predicates don't have a namespace.
serializeTag :: TagType -> Annotation -> String
serializeTag Open a
  = "<" ++ (rangeType a) ++ (serializeAttributes a) ++ ">"
serializeTag Close a
  = "</" ++ (rangeType a) ++ ">"
serializeTag Empty a
  = "<" ++ (rangeType a) ++ (serializeAttributes a) ++ "/>"

-- A serializer that creates tags of a chosen tagName and writes the
-- type into an rdf:a-attribute.
serializeSpanTag :: (Annotation -> String) -> String -> TagType -> Annotation -> String
serializeSpanTag slizeAttrs tagName Open a
  = "<" ++ tagName
    ++ " rdf:a=\"" ++ (rangeType a) ++ "\""
    ++ slizeAttrs a
    ++ ">"
serializeSpanTag _ tagName Close _
  = "</" ++ tagName ++ ">"
serializeSpanTag slizeAttrs tagName Empty a
  = "<" ++ tagName
    ++ " rdf:a=\"" ++ (rangeType a) ++ "\""
    ++ slizeAttrs a
    ++ "/>"

serializeNsTag :: (Annotation -> String) -> String -> TagType -> Annotation -> String
serializeNsTag slizeAttrs prefix Open a =
  "<" ++ prefix ++ ":" ++ (snd qName)
  ++ " xmlns:annot=\"" ++ (fst qName) ++ "\""
  ++ slizeAttrs a
  ++ ">"
  where qName = splitNamespaceName a
serializeNsTag _ prefix Close a =
  "</" ++ prefix ++ ":" ++ (snd $ splitNamespaceName a) ++ ">"
serializeNsTag slizeAttrs prefix Empty a =
  "<" ++ prefix ++ ":" ++ (snd qName)
  ++ " xmlns:annot=\"" ++ (fst qName) ++ "\""
  ++ slizeAttrs a
  ++ "/>"
  where qName = splitNamespaceName a
