module StandOff.Splitting
  ( splitExternal
  , merge
  , splitOverlapping
  )
where

import StandOff.TextRange
import StandOff.MarkupTree


-- | Make a list of non-overlapping ranges from a list of
-- (potentially) overlapping ranges by splitting overlapping
-- ranges. So the result is something, that could be represented as a
-- tree. We can look at it as if (quasi) it was a tree.
splitExternal :: (TextRange a) => [a] -> [a]
splitExternal = sortTextRanges . spltExtRec . sortTextRanges

spltExtRec :: (TextRange a) => [a] -> [a]
spltExtRec [] = []
spltExtRec (a:as)
  | (length splits) == 1 = a:(spltExtRec as)
  | otherwise =  spltExtRec splits++as
  where splits = spltOverlapping a as

spltOverlapping :: (TextRange a) => a -> [a] -> [a]
spltOverlapping x [] = [x]
spltOverlapping x (y:ys)
  | x `leftOverlaps` y = mkList $ leftSplit FstSplit x y
  | x `rightOverlaps` y = mkList $ rightSplit FstSplit x y
  | otherwise = spltOverlapping x ys
  where mkList (t1, t2) = [t1, t2]

-- | Not really merge, but SPLIT an annotation depending on the
-- 'Tree'. This function is the workhorse of markup
-- internalization. Splitting is only necessary in three situations:
-- overlapping, and lost tags, i.e. only one of the pair of tags is
-- within the range of the external markup. Also in some situations
-- when the external markup extends into a tag, however we first try
-- to keep at least a split, then.
merge :: (MarkupTree t b, TextRange b, TextRange a) => [t b] -> a -> [a]
merge [] a = [a]
merge (x:xs) a
  -- If a spans the equal range as x, then return a.
  | a `spansEq` (getNode x) = [a]
  -- Needn't progress behind a.
  | a `before` (getNode x) = [a]
  -- Forward xml horizontally when a is behind x
  | a `behind` (getNode x) = (merge xs a)
  -- a is in a single tag of x: drop it
  | a `inTag` (getNode x) = []
  -- a contained in x and it starts in a forbidden position, i.e. in
  -- the opening tag of x (lost tag):
  | (getNode x) `contains` a && a `startLeftForbidden` (getNode x) = merge (x:xs) $ snd $ leftSplit SndSplit a (getNode x)
  -- a contained in x and it ends in a forbidden position, i.e. in the
  -- closing tag of x (lost tag):
  | (getNode x) `contains` a && a `endRightForbidden` (getNode x) = merge (x:xs) $ fst $ rightSplit FstSplit a (getNode x)
  -- a's end extends into a closing tag
  | a `endLeftForbidden` (getNode x) = merge (x:xs) $ fst $ leftSplit FstSplit a (getNode x)
  -- a's start extends into an opening tag
  | a `startRightForbidden` (getNode x) = merge (x:xs) $ snd $ rightSplit SndSplit a (getNode x)
  -- Split a when a right-overlaps x.
  | a `rightOverlaps` (getNode x) =
    (merge (getMarkupChildren x) (fst rightSplit')) ++ (merge xs (snd rightSplit'))
  -- Split a when a left-overlaps x.
  | a `leftOverlaps` (getNode x) =
    (fst leftSplit') : (merge (getMarkupChildren x) (snd leftSplit'))
  -- Forward xml vertically when x contains a
  | (getNode x) `contains` a = merge (getMarkupChildren x) a
  -- If a contains x, proceed with xs:
  | a `contains` (getNode x) = merge xs a
  | otherwise = error "Could not resolve overlapping!"
  where
    rightSplit' = rightSplit FstSplit a (getNode x)
    leftSplit' = leftSplit FstSplit a (getNode x)

-- | Make external markup internalizable by splitting it with itself
-- and a 'Tree'.
splitOverlapping :: (MarkupTree t b, TextRange b, TextRange a) => [t b] -> [a] -> [a]
splitOverlapping internal = concatMap (merge internal) . splitExternal
