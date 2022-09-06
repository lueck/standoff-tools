{-# LANGUAGE MultiParamTypeClasses #-}
module StandOff.MarkupTree
  ( MarkupTree(..)
  , AnnotationCond(..)
  , RestrictedForrest(..)
  , annotationsOnRestrictedTrees
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


-- | A type for expressing if some part of a document (a tree) is
-- restriced regarding annotations. 'Restricted' marks a tree to be
-- restricted, 'Annotatable' marks a tree to be free to
-- annotations. E.g. the prolog in an XML document should be
-- restriced, but annotations are allowed in the root element.
data AnnotationCond t
  -- | A restricted tree.
  = Restricted t
  -- | A tree that can be annotated.
  | Annotatable t
  deriving (Eq, Show)

isRestricted :: AnnotationCond a -> Bool
isRestricted (Restricted _tree) = True
isRestricted _ = False

-- | A markup language's tree should be an instance of this class in
-- order to express restricted areas for annotation.
class (MarkupTree t n, TextRange n) => RestrictedForrest t n where
  -- | Return the forrest with restriction markers.
  treeConditions :: [t n] -> [(AnnotationCond (t n))]


restrictedSpans :: (MarkupTree t n, RestrictedForrest t n,  TextRange n) => [t n] -> [n]
restrictedSpans forrest =
  map (getNode . fst) $
  filter (\(_t, cond) -> isRestricted cond) $
  zip forrest $ treeConditions forrest

-- | Test whether any of the annotations extends into the restricted
-- spans of a 'RestrictedForrest'. Returns an error message or False.
annotationsOnRestrictedTrees
  :: (TextRange a, Show a, TextRange n, RestrictedForrest t n) =>
     [a]   -- ^ annotations
  -> [t n] -- ^ restricted forrest
  -> Either String Bool
annotationsOnRestrictedTrees annots forrest =
  fmap (any id) $ traverse failOnRestrictedTree annots
  where
    failOnRestrictedTree :: (TextRange a, Show a) => a -> Either String Bool
    failOnRestrictedTree annot =
      case annot `touchesAny` (restrictedSpans forrest) of
        True -> Left $ "Annotation extends into restricted span: " ++ show annot
        False -> Right False
