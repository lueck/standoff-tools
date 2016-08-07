module StandOff.Internalizer.Internalize
  ( internalize
  ) where

import StandOff.Data.XML
import StandOff.Data.Annotation
import StandOff.XML.LineOffsets
import StandOff.Internalizer.ResolveOverlapping
import StandOff.Internalizer.TagSerializer

-- Internalize
internalize :: String -> [XML] -> [Annotation] -> (TagType -> Annotation -> String) -> String
internalize doc xml annotations serializer =
  insertTags (concatMap (resolveOverlapping elems) markupRanges) serializer doc 1
  where elems = filter (isElementP) xml
        markupRanges = makeQuasiTree $ filter isMarkupRangeP annotations

-- Split Annotations depending on XML in source file. This function is
-- the workhorse of markup internalization.
resolveOverlapping :: [XML] -> Annotation -> [Annotation]
resolveOverlapping [] a = [a]
resolveOverlapping (x:xs) a
  -- Forward xml vertically when x contains a
  | xStartOpen <= aStart && xEndClose >= aEnd = resolveOverlapping xContent a
  -- Forward xml horizontally when x is before a
  | xEndClose <= aStart  = resolveOverlapping xs a
  -- Split a when a right-overlaps x
  | aStart > xStartOpen && aEnd > xEndClose = (resolveOverlapping xContent (fst rightSplit))
                                              ++ (resolveOverlapping xs (snd rightSplit))
  -- Split a when a left-overlaps x
  | aStart < xStartOpen && aEnd < xEndClose = (fst leftSplit)
                                              : (resolveOverlapping xContent (snd leftSplit))
  -- Needn't progress if a contains x, because then xs are not
  -- relevant and a contains the content of x, too.
  | aStart <= xStartOpen && aEnd >= xEndClose = [a]
  -- Needn't progress behind a.
  | aEnd <= xStartOpen = [a]
  | otherwise = error "Could not resolve overlapping!"
  where aStart = rangeStartOffset a
        aEnd = rangeEndOffset a
        xStartOpen = (posOffset (fst (xmlSpanning x)))
        xEndClose = (posOffset (snd (xmlSpanning x)))
        xContent = filter (isElementP) $ elementContent x
        leftSplit = splitAnnotationAtOpenTag a x
        rightSplit = splitAnnotationAtCloseTag a x

-- FIXME: add attribute to split annotations with information about split
splitAnnotationAtOpenTag :: Annotation -> XML -> (Annotation, Annotation)
splitAnnotationAtOpenTag a x = splitMarkupRange a newAttrs splitEnd splitRestart
   where xOpenTagPos = elementOpenTagPosition x
         splitEnd = posOffset $ fst xOpenTagPos
         splitRestart = (posOffset $ snd xOpenTagPos) + 1
         newAttrs = []

-- FIXME: add attribute to split annotations with information about split
splitAnnotationAtCloseTag :: Annotation -> XML -> (Annotation, Annotation)
splitAnnotationAtCloseTag a x = splitMarkupRange a newAttrs splitEnd splitRestart
   where xCloseTagPos = elementCloseTagPosition x
         splitEnd = posOffset $ fst xCloseTagPos
         splitRestart = (posOffset $ snd xCloseTagPos) + 1
         newAttrs = []

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
