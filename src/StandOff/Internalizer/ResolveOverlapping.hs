module StandOff.Internalizer.ResolveOverlapping where

import StandOff.Data.TextRange

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
