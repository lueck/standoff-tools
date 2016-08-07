module StandOff.Data.Tree where

-- A Tree is a recursive structure, like an XML element node.
class Tree a where
  contents :: a -> [a] -- returns the contents of a tree element
