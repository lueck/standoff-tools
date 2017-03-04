{-# LANGUAGE DeriveGeneric #-}

module StandOff.AnnotationTypeDefs
  where

import qualified Data.Map as Map
import GHC.Generics
import Data.UUID (UUID)
import Data.Aeson
import Data.UUID.Aeson

import StandOff.TextRange

data Annotation = MarkupRange { rangeId :: Maybe UUID
                              , elementId :: UUID
                              , markupType :: String
                              , startOffset :: Int
                              , endOffset :: Int
                              , text :: String
                              , attributes :: Map.Map String [String] }
                | Relation { relationId :: UUID
                           , subject :: UUID
                           , predicate :: String
                           , object :: UUID }
                deriving (Show, Generic)

instance TextRange (Annotation) where
  start (MarkupRange _ _ _ s _ _ _) = s
  end (MarkupRange _ _ _ _ e _ _) = e
  split (MarkupRange rid eid typ s1 e2 txt attrs) (e1, s2)
    = ( (MarkupRange rid eid typ s1 e1 txt attrs)
      , (MarkupRange rid eid typ s2 e2 txt attrs))
      -- FIXME: add attributes with info about split

instance ToJSON Annotation where
  toEncoding = genericToEncoding defaultOptions


rangeRangeId :: Annotation -> Maybe UUID
rangeRangeId (MarkupRange rid _ _ _ _ _ _) = rid

rangeElementId :: Annotation -> UUID
rangeElementId (MarkupRange _ eid _ _ _ _ _) = eid

rangeType :: Annotation -> String
rangeType (MarkupRange _ _ typ _ _ _ _) = typ

rangeStartOffset :: Annotation -> Int
rangeStartOffset (MarkupRange _ _ _ s _ _ _) = s

rangeEndOffset :: Annotation -> Int
rangeEndOffset (MarkupRange _ _ _ _ e _ _) = e

rangeAttributes :: Annotation -> Map.Map String [String]
rangeAttributes (MarkupRange _ _ _ _ _ _ attrs) = attrs


-- Predicates to be used by filters.

isMarkupRangeP :: Annotation -> Bool
isMarkupRangeP (MarkupRange _ _ _ _ _ _ _) = True
isMarkupRangeP _ = False

isRelationP :: Annotation -> Bool
isRelationP (Relation _ _ _ _) = True
isRelationP _ = False

isPredicateP :: Annotation -> Bool
isPredicateP _ = False


-- handling attributes

insertAttributeWith :: Annotation -> (String, String) -> Annotation
insertAttributeWith (MarkupRange rid eid typ s e txt attrs) (k, v) =
  (MarkupRange rid eid typ s e txt (Map.insertWith (++) k [v] attrs))
insertAttributeWith a _ = a

makeAttributiveRanges :: [Annotation] -> [Annotation]
makeAttributiveRanges as = ranges
  where ranges = filter isMarkupRangeP as
