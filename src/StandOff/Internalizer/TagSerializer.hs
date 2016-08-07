module StandOff.Internalizer.TagSerializer where

import StandOff.Data.Annotation

data TagType = Open | Close | Empty deriving (Show)

-- simple serializer for an XML tag. Only ok, if the range type does
-- not contain a namespace.
serializeTag :: TagType -> Annotation -> String
serializeTag Open a
  = "<"
    ++ (rangeType a)
    ++ " elementId=\"" ++ (rangeElementId a) ++ "\""
    ++ " rangeId=\"" ++ (rangeRangeId a) ++ "\""
    ++ (concatMap (\attr -> (" " ++ (rangeAttributeName attr)
                              ++ "=\"" ++ (rangeAttributeValue attr)
                              ++ "\""))
         (rangeAttributes a))
    ++ ">"
serializeTag Close a
  = "</"
    ++ (rangeType a)
    ++ ">"
serializeTag Empty a
  = "<"
    ++ (rangeType a)
    ++ " elementId=\"" ++ (rangeElementId a) ++ "\""
    ++ " rangeId=\"" ++ (rangeRangeId a) ++ "\""
    ++ (concatMap (\attr -> (" " ++ (rangeAttributeName attr)
                              ++ "=\"" ++ (rangeAttributeValue attr)
                              ++ "\""))
         (rangeAttributes a))
    ++ ">"

-- A serializer that creates tags of a chosen tagName and writes the
-- type into an rdf:a-attribute.
serializeSpanTag :: String -> TagType -> Annotation -> String
serializeSpanTag tagName Open a
  = "<" ++ tagName
    ++ " rdf:a=\"" ++ (rangeType a) ++ "\""
    ++ " elementId=\"" ++ (rangeElementId a) ++ "\""
    ++ " rangeId=\"" ++ (rangeRangeId a) ++ "\""
    ++ (concatMap (\attr -> (" " ++ (rangeAttributeName attr)
                              ++ "=\"" ++ (rangeAttributeValue attr)
                              ++ "\""))
         (rangeAttributes a))
    ++ ">"
serializeSpanTag tagName Close _
  = "</" ++ tagName ++ ">"
serializeSpanTag tagName Empty a
  = "<" ++ tagName
    ++ " rdf:a=\"" ++ (rangeType a) ++ "\""
    ++ " elementId=\"" ++ (rangeElementId a) ++ "\""
    ++ " rangeId=\"" ++ (rangeRangeId a) ++ "\""
    ++ (concatMap (\attr -> (" " ++ (rangeAttributeName attr)
                              ++ "=\"" ++ (rangeAttributeValue attr)
                              ++ "\""))
         (rangeAttributes a))
    ++ ">"
