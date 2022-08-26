module StandOff.XTraverse
where

import Data.Tree.Class
import Control.Monad.Trans
import Control.Monad.State

-- * Traversing an XML tree with state

-- | An in-order traversal with one function applied on the open tag
-- and an other function applied the close tag.
--
-- These functions take a node and a state and return an tuple of x
-- and a new state, where x is passed to a monadic
-- function. Typically, x is a string like type and the monadic
-- function prints it to IO. In such a case, the two functions map a
-- node into a string like object and a new state.
--
-- > final <- execStateT (mapM_ (xtraverse_ putStr f g) xml) initialState
-- > return final
xtraverse_
  :: (Monad m, Tree t) =>
     (a -> m ())        -- ^ monadic action on what a tag function returns
  -> (n -> s -> (a, s)) -- ^ the function applied on the open tag
  -> (n -> s -> (a, s)) -- ^ the function applied on the close tag
  -> t n                -- ^ the parsed 'XMLTree'
  -> StateT s m ()
xtraverse_ write_ f g xml = do
  state (f node) >>= (lift . write_)
  mapM_ (xtraverse_ write_ f g) $ getChildren xml
  state (g node) >>= (lift . write_)
  where
    node = getNode xml

-- | Execute a stateful in-order traversal on a 'Tree'. This applies
-- two state manipulating functions f and g on each node, f on the
-- open tag, g on the close tag. What f and g return is passed to a
-- monadic action, e.g. written to IO. The final state is returned.
xtraverseWithState
  :: (Monad m, Tree t) =>
     (a -> m ())        -- ^ monadic action for writing the result of a tag function
  -> (n -> s -> (a, s)) -- ^ the state manipulating function applied on the open tag
  -> (n -> s -> (a, s)) -- ^ the state manipulating function applied on the close tag
  -> [t n]              -- ^ the parsed XML document
  -> s                  -- ^ initial state
  -> m (s)
xtraverseWithState writeM f g xml s = execStateT (mapM_ (xtraverse_ writeM f g) xml) s


-- * More abstractions

-- | An abstraction of 'xtraverse_' for arbitrary monad transformers.
xtraverse'
  :: (Monad m, MonadTrans tr, Monad (tr m), Tree t) =>
     (a -> m c)        -- ^ monadic action on what a tag function returns
  -> (b -> tr m a)     -- ^ function wrapped around tag function in the monad transformer
  -> (n -> b)          -- ^ the function applied on the open tag
  -> (n -> b)          -- ^ the function applied on the close tag
  -> t n               -- ^ the parsed 'XMLTree'
  -> tr m (c, c)
xtraverse' write_ pipe f g xml = do
  o <- pipe (f node) >>= (lift . write_)
  mapM_ (xtraverse' write_ pipe f g) $ getChildren xml
  c <- pipe (g node) >>= (lift . write_)
  return (o, c)
  where
    node = getNode xml
