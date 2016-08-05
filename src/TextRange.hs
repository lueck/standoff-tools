module TextRange where

type Position = Int

-- A new instance of TextRange just has to define `start`, `end`,
-- `split` and maybe `splitPoints`.
class TextRange a where
  (<<>>) :: a -> a -> Bool -- contains
  (<-<) :: a -> a -> Bool -- left-overlaps
  (>->) :: a -> a -> Bool -- right-overlaps
  (<><) :: a -> a -> Bool -- before
  (><>) :: a -> a -> Bool -- behind
  start :: a -> Position -- start position
  end :: a -> Position -- end position
  spans :: a -> (Position, Position) -- tuple of start and end position
  len :: a -> Int -- length
  split :: a -> (Position, Position) -> (a, a)
  splitPoints :: a -> ((Position, Position), (Position, Position))
  leftSplit :: a -> a -> (a, a)
  rightSplit :: a -> a -> (a, a)
  x <<>> y = (start x <= start y) && (end x >= end y)
  x <-< y = (start x < start y) && (end x < end y)
  x >-> y = (start x > start y) && (end x > end y)
  x <>< y = (end x <= start y)
  x ><> y = (start x >= end y)
  spans x = ((start x), (end x))
  len x = (end x) - (start x)
  splitPoints x = ((s, s), (e, e))
    where s = start x
          e = end x
  leftSplit x y = split x $ fst $ splitPoints y
  rightSplit x y = split x $ snd $ splitPoints y
