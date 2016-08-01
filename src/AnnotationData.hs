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

rangeStartOffset :: Annotation -> Int
rangeStartOffset (MarkupRange _ _ _ s _ _) = s

rangeEndOffset :: Annotation -> Int
rangeEndOffset (MarkupRange _ _ _ _ e _) = e
