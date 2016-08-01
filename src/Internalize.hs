module Internalize where

import XMLData
import AnnotationData
import LineOffsets

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
  | otherwise = error "Did I miss something?"
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
