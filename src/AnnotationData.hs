module AnnotationData
  where

data Annotation = MarkupRange { rangeId :: String
                              , elementId :: String
                              , markupType :: String
                              , startOffset :: Int
                              , endOffset :: Int
                              , text :: String }
                | Relation { subject :: String
                           , predicate :: String
                           , object :: String }
                deriving (Show)
