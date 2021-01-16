module StandOff.MarkupTree
  ( MarkupTree(..)
  )
where

-- | An n-ary tree with markup, i.e. a rose tree like XML. The type
-- for representing internal markup must be an instance of this class.
class MarkupTree a where
  -- | Return the children of a tree node, which are markup
  -- elements. This should not return text nodes.
  getMarkupChildren :: a -> [a]
