module StandOff.ResolveOverlapping where

import StandOff.TextRange
import StandOff.Tree


-- | Make a list of non-overlapping ranges from a list of
-- (potentially) overlapping ranges by splitting overlapping
-- ranges. So the result is something, that could be represented as a
-- tree. We can look at it as if (quasi) it was a tree.
makeQuasiTree :: (TextRange a) => [a] -> [a]
makeQuasiTree as = sortTextRanges $ makeQuasiTree' sorted
  where sorted = sortTextRanges as

makeQuasiTree' :: (TextRange a) => [a] -> [a]
makeQuasiTree' [] = []
makeQuasiTree' (a:as)
  | (length splits) == 1 = a:(makeQuasiTree' as)
  | otherwise =  makeQuasiTree' splits++as
  where splits = splitOverlapping a as

splitOverlapping :: (TextRange a) => a -> [a] -> [a]
splitOverlapping x [] = [x]
splitOverlapping x (y:ys)
  | x `leftOverlaps` y = mkList $ leftSplit x y
  | x `rightOverlaps` y = mkList $ rightSplit x y
  | otherwise = splitOverlapping x ys
  where mkList (t1, t2) = [t1, t2]

-- Not really merge, but SPLIT an annotation depending on Tree. This
-- function is the workhorse of markup internalization.  We can not
-- use a number of functions from TextRange, like `behind`, because
-- the first and second argument's values will be of different
-- types. So we can only use TextRange's start and end, which return
-- Ints, which can then be compared like in TextRange.
merge :: (Tree b, TextRange b, TextRange a) => [b] -> a -> [a]
merge [] a = [a]
merge (x:xs) a
  -- If a spans the equal range as x, then return a.
  | a `spansEq` x = [a]
  -- a contained in x and it starts in a forbidden position, i.e. in
  -- the opening tag of x:
  | x `contains` a && a `startLeftForbidden` x = merge (x:xs) $ snd $ leftSplit a x
  -- a contained in x and it ends in a forbidden position, i.e. in the
  -- closing tag of x:
  | x `contains` a && a `endRightForbidden` x = merge (x:xs) $ fst $ rightSplit a x
  -- Split a when a right-overlaps x.
  | a `rightOverlaps` x =
    (merge (contents x) (fst rightSplit')) ++ (merge xs (snd rightSplit'))
  -- Split a when a left-overlaps x.
  | a `leftOverlaps` x =
    (fst leftSplit') : (merge (contents x) (snd leftSplit'))
  -- Forward xml vertically when x contains a
  | x `contains` a = merge (contents x) a
  -- Forward xml horizontally when a is behind x
  | a `behind` x = (merge xs a)
  -- If a contains x, proceed with xs:
  | a `contains` x = merge xs a
  -- Needn't progress behind a.
  | a `before` x = [a]
  | otherwise = error "Could not resolve overlapping!"
  where
    rightSplit' = rightSplit a x
    leftSplit' = leftSplit a x
