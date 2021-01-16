module StandOff.TextRange
  ( Position
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
-- startLeftForbidden x y = start x > fst sp && start x < snd sp
--   where sp = fst $ splitPoints y

-- | start of x between right split points
startRightForbidden :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
startRightForbidden x y = forbidden' start snd x y
-- startRightForbidden x y = start x > fst sp && start x < snd sp
--   where sp = snd $ splitPoints y

-- | end of x between left split points
endLeftForbidden :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
endLeftForbidden x y = forbidden' end fst x y
-- endLeftForbidden x y = end x > fst sp && end x < snd sp
--   where sp = fst $ splitPoints y

-- | end of x between right split points
endRightForbidden :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
endRightForbidden x y = forbidden' end snd x y
-- endRightForbidden x y = end x > fst sp && end x < snd sp
--   where sp = snd $ splitPoints y

-- | start or end of a is in a forbidden span between split points
forbidden :: (TextRange a1, TextRange a2) => a1 -> a2 -> Bool
forbidden x y = startLeftForbidden x y || startRightForbidden x y || endLeftForbidden x y || endRightForbidden x y

-- forbidden :: (TextRange a1, TextRange a2) => (a1 -> Position) -> ((b, b) -> b) -> a1 -> a2 -> Bool
forbidden' :: (TextRange a1, TextRange a2) =>
             (a1 -> Position) -- ^ 'start' or 'end'
          -> (((Position, Position), (Position, Position)) -> (Position,Position)) -- ^ 'fst' or 'snd'
          -> a1 -> a2 -> Bool
forbidden' xPt ySplPts x y = xPt x > fst spltPts && xPt x < snd spltPts
  where spltPts = ySplPts $ splitPoints y

-- | left-split first range by second range
leftSplit :: (TextRange a1, TextRange a2) => a1 -> a2 -> (a1, a1)
leftSplit x y = split x $ fst $ splitPoints y

-- | right-split first range by second range
rightSplit :: (TextRange a1, TextRange a2) => a1 -> a2 -> (a1, a1)
rightSplit x y = split x $ snd $ splitPoints y

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
