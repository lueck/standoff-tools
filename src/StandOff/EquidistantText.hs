module StandOff.EquidistantText
where

import Data.Tree.Class
import Control.Monad.State

import StandOff.StringLike
import StandOff.XTraverse


-- | The first in the resulting tuple is the string to be printed for
-- this node, the second is the new state.
class EquidistantNode n where
  serializeOpen :: (StringLike s) => Char -> n -> s -> (s, s)
  serializeClose :: (StringLike s) => Char -> n -> s -> (s, s)


equidistantText
  :: (StringLike s, Monoid s, Monad m, Tree t, EquidistantNode n) =>
     (s -> m ())  -- ^ monadic action
  -> Char         -- ^ the filling character
  -> [t n]        -- ^ the parsed XML document
  -> s            -- ^ the XML document as a string
  -> m ()
equidistantText writeM fillChar xml s = do
  final <- execStateT (mapM_ (xtraverse_ writeM f g) xml) s
  writeM final
  -- Should we do a test on the final state or write what is left over?
  where
    f = serializeOpen fillChar
    g = serializeClose fillChar
