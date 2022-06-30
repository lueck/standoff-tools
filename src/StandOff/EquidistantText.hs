module StandOff.EquidistantText
where

import Data.Tree.Class
import Control.Monad.State

import StandOff.StringLike


-- | The first in the resulting tuple is the string to be printed for
-- this node, the second is the new state.
class EquidistantNode n where
  serializeOpen :: (StringLike s) => Char -> n -> s -> (s, s)
  serializeClose :: (StringLike s) => Char -> n -> s -> (s, s)


-- | An in-order traversal with one monadic function applied on the
-- open tag and an other monadic function applied the close tag.
xtraverse_
  :: (Monad m, Tree t, StringLike s) =>
     (s -> m ())
  -> (n -> s -> (s, s)) -- ^ the function applied on the open tag
  -> (n -> s -> (s, s)) -- ^ the function applied on the close tag
  -> t n           -- ^ the parsed 'XMLTree'
  -> StateT s m ()
xtraverse_ write_ f g xml = do
  state (f node) >>= (lift . write_)
  mapM_ (xtraverse_ write_ f g) $ getChildren xml
  state (g node) >>= (lift . write_)
  where
    node = getNode xml


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
