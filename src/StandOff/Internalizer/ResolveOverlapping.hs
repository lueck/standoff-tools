module StandOff.Internalizer.ResolveOverlapping where

import StandOff.Data.TextRange
import StandOff.Data.Tree

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
  -- Split a when a right-overlaps x. FIXME: && start a < end a
  | start a > start x && end a >= end x && start a < end x =
    (merge (contents x) (fst rightSplit')) ++ (merge xs (snd rightSplit'))
  -- Split a when a left-overlaps x. FIXME: && end a > start x
  | start a <= start x && end a < end x && end a > start x =
    (fst leftSplit') : (merge (contents x) (snd leftSplit'))
  -- Forward xml vertically when x contains a
  | start x <= start a && end x >= end a = merge (contents x) a
  -- Forward xml horizontally when x is before a
  | end x <= start a = (merge xs a)
  -- Needn't progress if a contains x, because then xs are not
  -- relevant and a contains the content of x, too.
  | start a <= start x && end a >= end x  = merge xs a
  -- Needn't progress behind a.
  | end a <= start x = [a]
  | otherwise = error "Could not resolve overlapping!"
  where rightSplit' = split a $ snd $ splitPoints'
        leftSplit' = split a $ fst $ splitPoints'
        splitPoints' = correctSndStart $ splitPoints x
        -- split points have to be corrected because the second part
        -- of the split should always start right after a tag, but not
        -- at the position of the tags last char.
        correctSndStart ((f1, f2), (s1, s2)) = ((f1, f2+1), (s1, s2+1))
