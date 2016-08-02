module Internalize
  ( internalize
  , TagType
  , serializeTag
  ) where

import XMLData
import AnnotationData
import LineOffsets

data TagType = Open | Close | Empty deriving (Show)

-- Internalize
internalize :: String -> [XML] -> [Annotation] -> (TagType -> Annotation -> String) -> String
internalize doc xs as serializer =
  insertTags (resolveOverlapping (filter (isTagP) (flatten xs)) as) serializer doc 0

-- Split Annotations depending on internalized XML. 
resolveOverlapping :: [XML] -> [Annotation] -> [Annotation]
resolveOverlapping []  as = as
resolveOverlapping _ [] = []
resolveOverlapping (x:xs) (a:as)
  -- a contains x, where a is an annotation and x is internalized XML.
  | aStart <= xStartOpen && aEnd >= xEndClose = a : (resolveOverlapping (x:xs) as)
  -- x contains a
  | xStartOpen <= aStart && xEndClose >= aEnd = resolveOverlapping xs (a:as)
  -- a is before x
  | aEnd <= xStartOpen = a : resolveOverlapping (x:xs) as
  -- a is after x
  | aStart >= xEndClose = (resolveOverlapping (x:xs) as) ++ a:[]
  -- a left-overlaps x
  | aStart < xStartOpen && aEnd < xEndClose = (fst leftSplit) : (resolveOverlapping (x:xs) ((snd leftSplit):as))
  -- a right-overlaps x
  | aStart < xStartOpen && aEnd < xEndClose = resolveOverlapping (x:xs) ((fst rightSplit):(snd rightSplit):as)
  | otherwise = error "Could not resolve overlapping!"
  where aStart = rangeStartOffset a
        aEnd = rangeEndOffset a
        xStartOpen = (posOffset (fst (xmlSpanning x)))
        xEndClose = (posOffset (snd (xmlSpanning x)))
        leftSplit = splitAnnotationAtOpenTag a x
        rightSplit = splitAnnotationAtCloseTag a x

-- FIXME: add attribute to split annotations with information about split
splitAnnotationAtOpenTag :: Annotation -> XML -> (Annotation, Annotation)
splitAnnotationAtOpenTag a x = splitMarkupRange a newAttrs splitEnd splitRestart
   where xOpenTagPos = elementOpenTagPosition x
         splitEnd = posOffset $ fst xOpenTagPos
         splitRestart = (posOffset $ snd xOpenTagPos ) - 1
         newAttrs = []

-- FIXME: add attribute to split annotations with information about split
splitAnnotationAtCloseTag :: Annotation -> XML -> (Annotation, Annotation)
splitAnnotationAtCloseTag a x = splitMarkupRange a newAttrs splitEnd splitRestart
   where xCloseTagPos = elementCloseTagPosition x
         splitEnd = posOffset $ fst xCloseTagPos
         splitRestart = (posOffset $ snd xCloseTagPos) - 1
         newAttrs = []

-- Returns a list made from the tree.
flatten :: [XML] -> [XML]
flatten [] = []
flatten (x:xs) = (elementWithoutContent x) : (flatten $ elementContent x) ++ flatten xs

-- This actually does the job of inserting tags.
insertTags :: [Annotation] -> (TagType -> Annotation -> String) -> String -> Int -> String
insertTags as slize [] idx =
  concatMap (slize Empty) (filter (\a -> ((rangeStartOffset a) >= idx)
                                         && ((rangeStartOffset a) == (rangeEndOffset a))) as)
  ++ concatMap (slize Open) (filter (\a -> ((rangeStartOffset a) >= idx)
                                           && ((rangeStartOffset a) < (rangeEndOffset a))) as)
  ++ concatMap (slize Close) (filter (\a -> ((rangeStartOffset a) < idx)
                                             && ((rangeEndOffset a) >= idx)) as)
insertTags as slize (x:xs) idx =
  (concatMap (slize Empty) (filter (\a -> ((rangeStartOffset a) == idx)
                                          && ((rangeEndOffset a) == idx)) as))
  ++ (concatMap (slize Close) (filter (\a -> ((rangeStartOffset a) < idx)
                                             && ((rangeEndOffset a) == idx)) as))
  ++ (concatMap (slize Open) (filter (\a -> ((rangeStartOffset a) == idx)
                                            && ((rangeEndOffset a) > idx)) as))
  ++ (x : insertTags as slize xs (idx+1))

-- simple serializer for an XML tag
serializeTag :: TagType -> Annotation -> String
serializeTag Open a = "<"
                      ++ (rangeType a)
                      ++ " elementId=\"" ++ (rangeElementId a) ++ "\""
                      ++ " rangeId=\"" ++ (rangeRangeId a) ++ "\""
                      ++ (concatMap (\attr -> (" " ++ (rangeAttributeName attr)
                                               ++ "=\"" ++ (rangeAttributeValue attr)
                                               ++ "\""))
                           (rangeAttributes a))
                      ++ ">"
serializeTag Close a = "</"
                       ++ (rangeType a)
                       ++ ">"
serializeTag Empty a = "<"
                       ++ (rangeType a)
                       ++ " elementId=\"" ++ (rangeElementId a) ++ "\""
                       ++ " rangeId=\"" ++ (rangeRangeId a) ++ "\""
                       ++ (concatMap (\attr -> (" " ++ (rangeAttributeName attr)
                                                ++ "=\"" ++ (rangeAttributeValue attr)
                                                ++ "\""))
                            (rangeAttributes a))
                       ++ ">"
