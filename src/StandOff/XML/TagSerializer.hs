module StandOff.XML.TagSerializer where

import Data.List

import StandOff.Data.Annotation
import StandOff.Data.Tag

serializeAttributes :: Annotation -> String
serializeAttributes a =
  " elementId=\"" ++ (rangeElementId a) ++ "\""
  ++ " rangeId=\"" ++ (rangeRangeId a) ++ "\""
  ++ (concatMap (\attr -> (" " ++ (rangeAttributeName attr)
                            ++ "=\"" ++ (rangeAttributeValue attr)
                            ++ "\""))
       (rangeAttributes a))

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

-- simple serializer for an XML tag. Only okay, if the range type does
-- not contain a namespace.
serializeTag :: TagType -> Annotation -> String
serializeTag Open a
  = "<" ++ (rangeType a) ++ (serializeAttributes a) ++ ">"
serializeTag Close a
  = "</" ++ (rangeType a) ++ ">"
serializeTag Empty a
  = "<" ++ (rangeType a) ++ (serializeAttributes a) ++ "/>"

-- A serializer that creates tags of a chosen tagName and writes the
-- type into an rdf:a-attribute.
serializeSpanTag :: String -> TagType -> Annotation -> String
serializeSpanTag tagName Open a
  = "<" ++ tagName
    ++ " rdf:a=\"" ++ (rangeType a) ++ "\""
    ++ serializeAttributes a
    ++ ">"
serializeSpanTag tagName Close _
  = "</" ++ tagName ++ ">"
serializeSpanTag tagName Empty a
  = "<" ++ tagName
    ++ " rdf:a=\"" ++ (rangeType a) ++ "\""
    ++ serializeAttributes a
    ++ "/>"

serializeNsTag :: String -> TagType -> Annotation -> String
serializeNsTag prefix Open a =
  "<" ++ prefix ++ ":" ++ (snd qName)
  ++ " xmlns:annot=\"" ++ (fst qName) ++ "\""
  ++ serializeAttributes a
  ++ ">"
  where qName = splitNamespaceName a
serializeNsTag prefix Close a =
  "</" ++ prefix ++ ":" ++ (snd $ splitNamespaceName a) ++ ">"
serializeNsTag prefix Empty a =
  "<" ++ prefix ++ ":" ++ (snd qName)
  ++ " xmlns:annot=\"" ++ (fst qName) ++ "\""
  ++ serializeAttributes a
  ++ "/>"
  where qName = splitNamespaceName a
