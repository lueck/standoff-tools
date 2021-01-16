module StandOff.Tree
  ( MarkupTree(..)
  )
where

-- | An n-ary tree with markup, i.e. a rose tree like XML.
class MarkupTree a where
  -- | Return the children of a tree node, which are markup
  -- elements. This should not return text nodes.
  getMarkupChildren :: a -> [a]
