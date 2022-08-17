module StandOff.Utils
where

import qualified Data.Map.Strict as Map


-- | Traverse the keys of a 'Map.Map'. (If Map was a Bitraversable, we
-- would not need this.)
traverseKeys :: (Applicative f, Ord k2) => (k1 -> f k2) -> Map.Map k1 a -> f (Map.Map k2 a)
traverseKeys keyFun =
  fmap Map.fromList .
  traverse (\(k, v) -> (,) <$> keyFun k <*> pure v) .
  Map.toList
