module StandOff.TextRange
  ( Position
  , TextRange(..)
  -- * Analyse relative position of two ranges
  , (<<>>)
  , leftOverlaps
  , rightOverlaps
  , before
  , behind
  -- * Splitting
  , leftSplit
  , rightSplit
  -- * Sorting (Preprocessing)
  , sortTextRanges
  -- * Extra
  , len
  )
where

import Data.List

type Position = Int

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
  split :: a -> (Position, Position) -> (a, a)

  -- | return split points
  splitPoints :: a -> ((Position, Position), (Position, Position))
  splitPoints x = ((s, s), (e, e))
    where s = start x
          e = end x


-- | contains
(<<>>) :: TextRange a => a -> a -> Bool
x <<>> y = (start x <= start y) && (end x >= end y)

-- | left-overlaps
leftOverlaps :: TextRange a => a -> a -> Bool
leftOverlaps x y = (start x < start y) && (end x < end y) && (end x > start y)

-- | right-overlaps
rightOverlaps :: TextRange a => a -> a -> Bool
rightOverlaps x y = (start x > start y) && (end x > end y) && (start x < end y)

-- | before
before :: TextRange a => a -> a -> Bool
before x y = (end x <= start y)

-- | behind
behind :: TextRange a => a -> a -> Bool
behind x y = (start x >= end y)

-- | left-split first range by second range
leftSplit :: TextRange a => a -> a -> (a, a)
leftSplit x y = split x $ fst $ splitPoints y

-- | right-split first range by second range
rightSplit :: TextRange a => a -> a -> (a, a)
rightSplit x y = split x $ snd $ splitPoints y

-- | sort a list of text ranges
sortTextRanges :: (TextRange a) => [a] -> [a]
sortTextRanges ranges = sortBy compareRanges ranges
  where compareRanges x y
          | start x == start y = end y `compare` end x
          | otherwise = start x `compare` start y

-- DEPRECATED:
-- | length
len :: TextRange a => a -> Int
len x = (end x) - (start x)
