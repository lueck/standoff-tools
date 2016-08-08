module StandOff.Internalizer.Internalize
  ( internalize
  ) where

import StandOff.Data.TextRange
import StandOff.Data.XML
import StandOff.Data.Annotation
import StandOff.Data.Tag
import StandOff.Internalizer.ResolveOverlapping

-- Internalize
internalize :: String -> [XML] -> [Annotation] -> (TagType -> Annotation -> String) -> String
internalize doc xml annotations serializer =
  insertTags (concatMap (merge elems) markupRanges) serializer doc 1
  where elems = filter (isElementP) xml
        markupRanges = makeQuasiTree $ filter isMarkupRangeP annotations

-- This actually does the job of inserting tags. We have to revert the
-- list of closing tags for a position, as long as we deal with list
-- (quasi-tree) of annotations instead of trees.
insertTags :: [Annotation] -> (TagType -> Annotation -> String) -> String -> Int -> String
insertTags as slize [] idx =
  concatMap (slize Empty) (filter (\a -> ((rangeStartOffset a) >= idx)
                                         && ((rangeStartOffset a) == (rangeEndOffset a))) as)
  ++ concatMap (slize Open) (reverse (filter (\a ->
                                                ((rangeStartOffset a) >= idx)
                                                && ((rangeStartOffset a) < (rangeEndOffset a)))
                                      as))
  ++ concatMap (slize Close) (filter (\a -> ((rangeStartOffset a) < idx)
                                             && ((rangeEndOffset a) >= idx)) as)
insertTags as slize (x:xs) idx =
  (concatMap (slize Empty) (filter (\a -> ((rangeStartOffset a) == idx)
                                          && ((rangeEndOffset a) == idx)) as))
  ++ (concatMap (slize Close) (reverse (filter (\a -> ((rangeStartOffset a) < idx)
                                                      && ((rangeEndOffset a) == idx)) as)))
  ++ (concatMap (slize Open) (filter (\a -> ((rangeStartOffset a) == idx)
                                            && ((rangeEndOffset a) > idx)) as))
  ++ (x : insertTags as slize xs (idx+1))
