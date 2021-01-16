module StandOff.ResolveOverlapping {-# DEPRECATED "Use TextRange instead" #-} where

import StandOff.TextRange

makeQuasiTree :: (TextRange a) => [a] -> [a]
makeQuasiTree = splitExternal
{-# DEPRECATED makeQuasiTree "use StandOff.TextRange.splitExternal" #-}
