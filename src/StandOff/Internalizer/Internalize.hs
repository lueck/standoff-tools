module StandOff.Internalizer.Internalize
  ( internalize
  ) where

import StandOff.Data.TextRange
import StandOff.Data.Tree
import StandOff.Data.Tag
import StandOff.Internalizer.ResolveOverlapping

-- Internalize
internalize :: (TextRange a, TextRange b, Tree b) => String -> [b] -> [a] -> (TagType -> a -> String) -> String
internalize doc internal external serializer =
  insertTags (concatMap (merge internal) nestedInternal) serializer doc 1
  where nestedInternal = makeQuasiTree external

-- This actually does the job of inserting tags. We have to revert the
-- list of closing tags for a position, as long as we deal with list
-- (quasi-tree) of annotations instead of trees.
insertTags :: (TextRange a) => [a] -> (TagType -> a -> String) -> String -> Int -> String
insertTags as slize [] idx =
  concatMap (slize Empty) (filter (\a -> ((start a) >= idx)
                                         && ((start a) == (end a))) as)
  ++ concatMap (slize Open) (reverse (filter (\a -> ((start a) >= idx)
                                                    && ((start a) < (end a)))
                                      as))
  ++ concatMap (slize Close) (filter (\a -> ((start a) < idx)
                                             && ((end a) >= idx)) as)
insertTags as slize (x:xs) idx =
  (concatMap (slize Empty) (filter (\a -> ((start a) == idx)
                                          && ((end a) == idx)) as))
  ++ (concatMap (slize Close) (reverse (filter (\a -> ((start a) < idx)
                                                      && ((end a) == idx)) as)))
  ++ (concatMap (slize Open) (filter (\a -> ((start a) == idx)
                                            && ((end a) > idx)) as))
  ++ (x : insertTags as slize xs (idx+1))
