module Internalize where

import XMLData
import AnnotationData
import LineOffsets


-- Internalize
internalize :: String -> [XML] -> [Annotation] -> (Int -> Annotation -> String) -> String
internalize doc xs as serializer = insertTags (resolveOverlapping (flatten xs) as) serializer doc 0

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

-- FIXME: add attribute to splitted annotations with information about split
splitAnnotationAtOpenTag :: Annotation -> XML -> (Annotation, Annotation)
splitAnnotationAtOpenTag (MarkupRange rid eid typ s1 e1 _) (Element _ _ so eo _ _ _) =
   ((MarkupRange rid eid typ s1 (posOffset so) "")
   , (MarkupRange rid eid typ ((posOffset eo)-1) e1 ""))

-- FIXME: add attribute to splitted annotations with information about split
splitAnnotationAtCloseTag :: Annotation -> XML -> (Annotation, Annotation)
splitAnnotationAtCloseTag (MarkupRange rid eid typ s1 e1 _) (Element _ _ _ _ sc ec _) =
   ((MarkupRange rid eid typ s1 (posOffset sc) "")
   , (MarkupRange rid eid typ ((posOffset ec)-1) e1 ""))

-- Returns a list made from the tree.
flatten :: [XML] -> [XML]
flatten [] = []
flatten ((Element n a so se sc ec inner):xs) =
  (Element n a so se sc ec []) : flatten inner ++ flatten xs
flatten (x:xs) = x : flatten xs

-- This actually does the job of internalizing.
insertTags :: [Annotation] -> (Int -> Annotation -> String) -> String -> Int -> String
insertTags as slize [] idx = concatMap (slize idx) $ filter (\a -> ((rangeStartOffset a) >= idx) || ((rangeEndOffset a) >= idx)) as
insertTags as slize (x:xs) idx = (concatMap (slize idx) (filter (\a -> ((rangeStartOffset a) == idx) || ((rangeEndOffset a) == idx)) as)) ++ (x : insertTags as slize xs (idx+1))

-- FIXME: The serializer function takes an Integer and an Annotation
-- as arguments. The integer is an index, by which the serializer can
-- decide, whether to return an opening or a closing tag. -- This is
-- not good: 1) Makes it hard to write serializers. 2) There may be
-- situations, when the index does not equal start and end offsets,
-- e.g. see edge condition. All decision logic should be implemented
-- in insertTags, and the serializer should be called with
-- Start|Close|Empty instead of an integer index.


