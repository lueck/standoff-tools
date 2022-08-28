{-# LANGUAGE MultiParamTypeClasses #-}
module StandOff.MarkupTree
  ( MarkupTree(..)
  )
where

import StandOff.TextRange


-- | An n-ary tree with markup, i.e. a rose tree like XML. The type
-- for representing internal markup must be an instance of this
-- class. A markup tree is parametrized with a node type.
class (TextRange n) => MarkupTree t n where

  {-# MINIMAL getChildren, getNode #-}

  -- | Return the children of a tree node, which are markup
  -- elements. This should not return text nodes.
  getMarkupChildren :: t n -> [t n]
  getMarkupChildren = filter ((/=0) . length . restrictedRanges . getNode) . getChildren

  -- | Get the children of the (current subtree's) root.
  getChildren :: t n -> [t n]

  -- | Get the node of the (current subtree's) root.
  getNode :: t n -> n
