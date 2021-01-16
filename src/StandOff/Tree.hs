module StandOff.Tree
  ( Tree(..)
  )
where

-- | An n-ary tree, i.e. a rose tree like XML.
class Tree a where
  getChildren :: a -> [a] -- ^ returns the children of a tree node
