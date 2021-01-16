module StandOff.TextRange
  ( Position
  , MainSplit(..)
  , TextRange(..)
  -- * Analyse relative position of two ranges
  , contains
  , (<<>>)
  , leftOverlaps
  , rightOverlaps
  , before
  , behind
  , startsBefore
  , endsBehind
  , spansEq
  , startLeftForbidden
  , startRightForbidden
  , endLeftForbidden
  , endRightForbidden
  , forbidden
  -- * Splitting
  , leftSplit
  , rightSplit
  -- * Sorting (Preprocessing)
  , sortTextRanges
  -- * Resolve overlapping edges
  , splitExternal
  , merge
  -- * Extra
  , len
  )
where

import Data.List
import StandOff.Tree

-- | A position
type Position = Int

-- | A choice which of two text ranges returned by 'split' should be
-- the main one, e.g. have the ID. Most of the time, this is the first
-- split, but there are occasions, where the first split is dropped,
-- so the unique attributes of the text range must go to the second
-- split.
data MainSplit = FstSplit | SndSplit

-- | A range in a text given by start and end point.
class TextRange a where
  {-# MINIMAL start, end, split | spans, split #-}

   -- | start position
  start :: a -> Position
  start = fst . spans

  -- | end position
  end :: a -> Position
  end = snd . spans

  -- | tuple of start and end position
  spans :: a -> (Position, Position)
  spans x = ((start x), (end x))

  -- | split a range at a given positions (start and end of an other
  -- range) into two ranges
  split :: MainSplit -> a -> (Position, Position) -> (a, a)

  -- | return split points
  splitPoints :: a -> ((Position, Position), (Position, Position))
  splitPoints x = ((s, s), (e, e))
    where s = start x
          e = end x


-- | contains
contains :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
contains x y = (start x <= start y) && (end x >= end y)

-- | like 'contains', but iconic name
(<<>>) :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
x <<>> y = (start x <= start y) && (end x >= end y)
{-# DEPRECATED (<<>>) "Use contains" #-}

-- | left-overlaps
leftOverlaps :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
leftOverlaps x y = (start x < start y) && (end x < end y) && (end x > start y)

-- | right-overlaps
rightOverlaps :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
rightOverlaps x y = (start x > start y) && (end x > end y) && (start x < end y)

-- | before
before :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
before x y = (end x <= start y)

-- | behind
behind :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
behind x y = (start x >= end y)

-- | starts before
startsBefore :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
startsBefore x y = (start x) < (start y)

-- | ends behind
endsBehind :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
endsBehind x y = (end x) > (end y)

-- | start and end points equal
spansEq :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
spansEq x y = spans x == spans y

-- | start of x between left split points
startLeftForbidden :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
startLeftForbidden x y = forbidden' start fst x y

-- | start of x between right split points
startRightForbidden :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
startRightForbidden x y = forbidden' start snd x y

-- | end of x between left split points
endLeftForbidden :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
endLeftForbidden x y = forbidden' end fst x y

-- | end of x between right split points
endRightForbidden :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
endRightForbidden x y = forbidden' end snd x y

-- | start or end of a is in a forbidden span between split points
forbidden :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
forbidden x y = startLeftForbidden x y || startRightForbidden x y || endLeftForbidden x y || endRightForbidden x y

-- forbidden' :: (TextRange a1, TextRange a2) => (a1 -> Position) -> ((b, b) -> b) -> a1 -> a2 -> Bool
forbidden' :: (TextRange a1, TextRange a2) =>
              (a1 -> Position) -- ^ 'start' or 'end'
           -> (((Position, Position), (Position, Position)) -> (Position,Position)) -- ^ 'fst' or 'snd'
           -> a1 -> a2 -> Bool
forbidden' xPt ySplPts x y = xPt x > fst spltPts && xPt x < snd spltPts
  where spltPts = ySplPts $ splitPoints y

-- | left-split first range by second range
leftSplit :: (TextRange a1, TextRange a2) => MainSplit -> a1 -> a2 -> (a1, a1)
leftSplit ms x y = split ms x $ fst $ splitPoints y

-- | right-split first range by second range
rightSplit :: (TextRange a1, TextRange a2) => MainSplit -> a1 -> a2 -> (a1, a1)
rightSplit ms x y = split ms x $ snd $ splitPoints y

-- | sort a list of text ranges
sortTextRanges :: (TextRange a) => [a] -> [a]
sortTextRanges = sortBy compareRanges
  where compareRanges x y
          | start x == start y = end y `compare` end x
          | otherwise = start x `compare` start y


-- | length
len :: TextRange a => a -> Int
len x = (end x) - (start x)
{-# DEPRECATED len "Don't use len. It prevents making 'Position' abstract." #-}


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

-- | Not really merge, but SPLIT an annotation depending on Tree. This
-- function is the workhorse of markup internalization.
merge :: (MarkupTree b, TextRange b, TextRange a) => [b] -> a -> [a]
merge [] a = [a]
merge (x:xs) a
  -- If a spans the equal range as x, then return a.
  | a `spansEq` x = [a]
  -- a contained in x and it starts in a forbidden position, i.e. in
  -- the opening tag of x:
  | x `contains` a && a `startLeftForbidden` x = merge (x:xs) $ snd $ leftSplit SndSplit a x
  -- a contained in x and it ends in a forbidden position, i.e. in the
  -- closing tag of x:
  | x `contains` a && a `endRightForbidden` x = merge (x:xs) $ fst $ rightSplit FstSplit a x
  -- Split a when a right-overlaps x.
  | a `rightOverlaps` x =
    (merge (getMarkupChildren x) (fst rightSplit')) ++ (merge xs (snd rightSplit'))
  -- Split a when a left-overlaps x.
  | a `leftOverlaps` x =
    (fst leftSplit') : (merge (getMarkupChildren x) (snd leftSplit'))
  -- Forward xml vertically when x contains a
  | x `contains` a = merge (getMarkupChildren x) a
  -- Forward xml horizontally when a is behind x
  | a `behind` x = (merge xs a)
  -- If a contains x, proceed with xs:
  | a `contains` x = merge xs a
  -- Needn't progress behind a.
  | a `before` x = [a]
  | otherwise = error "Could not resolve overlapping!"
  where
    rightSplit' = rightSplit FstSplit a x
    leftSplit' = leftSplit FstSplit a x
