module StandOff.XTraverse
where

import Data.Tree.Class
import Control.Monad.State


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

