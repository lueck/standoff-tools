module AnnotationData
  where

import TextRange

data RangeAttribute = RangeAttribute { attrName :: String
                                     , attrValue :: String
                                     } deriving (Show)

rangeAttributeName :: RangeAttribute -> String
rangeAttributeName (RangeAttribute n _) = n

rangeAttributeValue :: RangeAttribute -> String
rangeAttributeValue (RangeAttribute _ v) = v

data Annotation = MarkupRange { rangeId :: String
                              , elementId :: String
                              , markupType :: String
                              , startOffset :: Int
                              , endOffset :: Int
                              , text :: String }
                | Relation { relationId :: String
                           ,  subject :: String
                           , predicate :: String
                           , object :: String }
                deriving (Show)

instance TextRange (Annotation) where
  start (MarkupRange _ _ _ s _ _) = s
  end (MarkupRange _ _ _ _ e _) = e
  split (MarkupRange rid eid typ s1 e2 txt) (e1, s2)
    = ( (MarkupRange rid eid typ s1 e1 txt)
      , (MarkupRange rid eid typ s2 e2 txt))

rangeRangeId :: Annotation -> String
rangeRangeId (MarkupRange rid _ _ _ _ _) = rid

rangeElementId :: Annotation -> String
rangeElementId (MarkupRange _ eid _ _ _ _) = eid

rangeType :: Annotation -> String
rangeType (MarkupRange _ _ typ _ _ _) = typ

rangeStartOffset :: Annotation -> Int
rangeStartOffset (MarkupRange _ _ _ s _ _) = s

rangeEndOffset :: Annotation -> Int
rangeEndOffset (MarkupRange _ _ _ _ e _) = e

rangeAttributes :: Annotation -> [RangeAttribute]
rangeAttributes (MarkupRange _ _ _ _ _ _) = []

splitMarkupRange :: Annotation -> [RangeAttribute] -> Int -> Int -> (Annotation, Annotation)
splitMarkupRange (MarkupRange rid eid typ s1 e2 txt) _ e1 s2 =
  ( (MarkupRange rid eid typ s1 e1 txt)
  , (MarkupRange rid eid typ s2 e2 txt))

-- Predicate to be used by filters.
isMarkupRangeP :: Annotation -> Bool
isMarkupRangeP (MarkupRange _ _ _ _ _ _) = True
isMarkupRangeP _ = False
