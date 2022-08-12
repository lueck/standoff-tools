module StandOff.EquidistantText
where

import Data.Tree.Class

import StandOff.StringLike
import StandOff.XTraverse


-- | The first in the resulting tuple is the string to be printed for
-- this node, the second is the new state.
class EquidistantNode n where
  -- | Make an equidistant representation of an opening tag. For XML
  -- this is the replacement for the opening tag of an non-empty
  -- element node, or a complete equidistant representation of any
  -- other node.
  serializeOpen
    :: (StringLike s) =>
       Char -- ^ the filling character
    -> n    -- ^ the current node
    -> s    -- ^ state: the portion of the input file not yet seen
    -> (s, s) -- ^ returns the equidistant representation of the
              -- current node and the new state, i.e. the portion of
              -- the input file not yet processed
  -- | Make an equidistant representation of a closing tag. For XML
  -- this is the empty string for every type of node but a non-empty
  -- element.
  serializeClose :: (StringLike s) => Char -> n -> s -> (s, s)


-- | Generate equidistant text.
--
-- Implementation notice: This traverses the XML tree with a
-- state. The state object is just the XML document as a string. The
-- current state is the portion of the string not yet processed. Each
-- node's equidistant string representation is written to a monad,
-- e.g. IO.
equidistantText
  :: (StringLike s, Monoid s, Monad m, Tree t, EquidistantNode n) =>
     (s -> m ())  -- ^ monadic action
  -> Char         -- ^ the filling character
  -> [t n]        -- ^ the parsed XML document
  -> s            -- ^ the XML document as a string
  -> m ()
equidistantText writeM fillChar xml s =
  xtraverseWithState writeM (serializeOpen fillChar) (serializeClose fillChar) xml s >>= writeM
