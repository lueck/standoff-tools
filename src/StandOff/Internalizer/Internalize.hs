module StandOff.Internalizer.Internalize
  ( internalize
  ) where

import StandOff.Data.TextRange
import StandOff.Data.Tree
import StandOff.Data.Tag
import StandOff.Internalizer.ResolveOverlapping

-- | Internalize external markup into a document.
--
-- Implementation:
--
-- 1) Split the annotations with each other, so that there are no
-- overlapping elemnts any more. This is done by a call to
-- 'makeQuasiTree', which returns a list of splitted annotations in
-- the right order.
--
-- 2) Split these splitted annotations again with regard to the markup
-- in the source document. After this, the annotations will not
-- overlap with the markup internal to the document. This is done by
-- calling 'merge', which takes a parsed xml tree from the document
-- and the list of annotations (splitted in step 1) and returns a list
-- of splitted annotations in the right order. The xml tree must be
-- elements with start and end character offsets.
--
-- 3) Insert the twice splitted annotations into the document given as
-- string. Here we only have to insert tags at the start and end
-- position given by the splitted annotations. This is done by calling
-- 'insertTags', which also takes care of the order of the closing
-- tags.
--
-- This implementation does not need a look-ahead parser. Instead all
-- work is done with lists, which is the strength of lispy
-- haskell. All can be done with pure functions--nice for testing.
internalize :: (TextRange a, TextRange b, Tree b) =>
               String  -- ^ the document
               -> [b]  -- ^ the parsed xml tree of the document
               -> [a]  -- ^ the annotations to be internalized
               -> (TagType -> a -> String) -- ^ the serializer for
                                           -- tags
               -> String -- ^ returned document with internalized
                         -- annotations
internalize doc internal external serializer =
  insertTags serializer tagsZipped doc 0
  where
    tagsZipped = zip (repeat Open) tagsMerged
    tagsMerged = concatMap (merge internal) nestedInternal
    nestedInternal = makeQuasiTree external

-- | 'insertTags' actually makes the job of inserting tags into the
-- document.
insertTags :: (TextRange a) =>
              (TagType -> a -> String) -- ^ tag serializer
           -> [(TagType, a)]           -- ^ annotations zipped with
                                       -- Open, startimg with tuples
                                       -- of (Open, a)
           -> String                   -- ^ document
           -> Int                      -- ^ index where in document,
                                       -- starting with 0
           -> String                   -- ^ Returned document with
                                       -- tags inserted.
insertTags _ [] doc _ = doc
insertTags slize ((tagType, a):as) doc idx
  -- insert empty tag
  | (start a) == (end a)
  = (take dStart doc) ++
    (slize Empty a) ++
    insertTags slize as (drop dStart doc) (start a)
  -- insert open tag
  | tagType == Open
  = (take dStart doc) ++
    (slize Open a) ++
    (insertTags slize (insClose as a) (drop dStart doc) (start a))
  -- insert close tag
  | tagType == Close
  = (take dEnd doc) ++
    (slize Close a) ++
    (insertTags slize as (drop dEnd doc) (end a))
  where
    dStart = (start a) - idx -- character delta of idx and start of a
    dEnd = (end a) - idx -- character delta

-- | Insert a Close tag to a list of tags.
insClose :: (TextRange a) => [(TagType, a)] -> a -> [(TagType, a)]
insClose [] endTag = [(Close, endTag)]
insClose (a@(tagTyp, annot):as) endTag
  | ( -- endTag left from start of a or exactly there
      ((end endTag) <= (start annot) && tagTyp == Open) ||
      -- endTag left from end of a. If endTag ends where a ends, then
      -- it has to be inserted after a, because a started before the
      -- element of endTag.
      ((end endTag) < (end annot) && tagTyp == Close))
  = (Close, endTag):a:as
  | otherwise = a : (insClose as endTag)

-- | Deprecated: Because using slow 'insertTags\''
--
-- Internalize external markup into a document.
internalize' :: (TextRange a, TextRange b, Tree b) => String -> [b] -> [a] -> (TagType -> a -> String) -> String
internalize' doc internal external serializer =
  insertTags' serializer (concatMap (merge internal) nestedInternal) doc 1
  where
    nestedInternal = makeQuasiTree external

-- | Deprecated: This function is very slow because it filters the
-- annotations for each char of the document! It would take
-- @tagsMerged@ in 'internalize'.
--
-- This actually does the job of inserting tags. We have to revert the
-- list of closing tags for a position, as long as we deal with list
-- (quasi-tree) of annotations instead of trees. With the list it is
-- really not performant, because we have to keep the list (at least
-- behind the close-tag) and filter it for each char of the
-- document. A real tree would improve performance, maybe.
insertTags' :: (TextRange a) => (TagType -> a -> String) -> [a] -> String -> Int -> String
insertTags' slize as [] idx =
  concatMap (slize Empty) (filter (\a -> ((start a) >= idx)
                                         && ((start a) == (end a))) as)
  ++ concatMap (slize Open) (reverse (filter (\a -> ((start a) >= idx)
                                                    && ((start a) < (end a)))
                                      as))
  ++ concatMap (slize Close) (filter (\a -> ((start a) < idx)
                                             && ((end a) >= idx)) as)
insertTags' slize as (x:xs) idx =
  (concatMap (slize Empty) (filter (\a -> ((start a) == idx)
                                          && ((end a) == idx)) as))
  ++ (concatMap (slize Close) (reverse (filter (\a -> ((start a) < idx)
                                                      && ((end a) == idx)) as)))
  ++ (concatMap (slize Open) (filter (\a -> ((start a) == idx)
                                            && ((end a) > idx)) as))
  ++ (x : insertTags' slize as xs (idx+1))
