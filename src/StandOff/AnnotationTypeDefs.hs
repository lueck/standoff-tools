{-# LANGUAGE DeriveGeneric #-}

module StandOff.AnnotationTypeDefs
  where

import qualified Data.Map as Map
import GHC.Generics
import Data.UUID (UUID)
import Data.Aeson
import Data.UUID.Aeson

import StandOff.TextRange

-- | Algebraic data type for annotations.
data Annotation
  = MarkupRange                  -- ^ range of characters in source file
    { rangeId :: Maybe UUID      -- ^ the UUID of the range
    , elementId :: UUID          -- ^ the UUID of the element the range is member of
    , markupType :: String       -- ^ the annotation type
    , startOffset :: Int         -- ^ the start character offset
    , endOffset :: Int           -- ^ the end character offset
    , text :: Maybe String       -- ^ the string between 'startOffset' and 'endOffset'
    , attributes :: Map.Map String [String] } -- ^ attributes of the range
  | Relation                     -- ^ a relation between two markup elements 
    { relationId :: UUID         -- ^ the UUID of the relation
    , subject :: UUID            -- ^ the UUID of the markup element
                                 -- which is the relation's subject
    , predicate :: String        -- ^ the relation's predicate
    , object :: UUID }           -- ^ the UUID of the markup element
                                 -- which is the relation's object
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

-- * Predicates to be used by filters.

-- | Returns true for a 'MarkupRange'.
isMarkupRangeP :: Annotation -> Bool
isMarkupRangeP (MarkupRange _ _ _ _ _ _ _) = True
isMarkupRangeP _ = False

-- | Returns true for a 'Relation'.
isRelationP :: Annotation -> Bool
isRelationP (Relation _ _ _ _) = True
isRelationP _ = False

-- | Returns true for a Predicate.
isPredicateP :: Annotation -> Bool
isPredicateP _ = False


-- * Getting the record fields of a 'MarkupRange'.
--
-- Patterns are not exhaustive. Filter before using!

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


-- * Handling attributes

-- | Insert an attribute to a markup range's attribute list.
insertAttributeWith :: Annotation -> (String, String) -> Annotation
insertAttributeWith (MarkupRange rid eid typ s e txt attrs) (k, v) =
  (MarkupRange rid eid typ s e txt (Map.insertWith (++) k [v] attrs))
insertAttributeWith a _ = a

-- | This actually only filters markup ranges.
makeAttributiveRanges :: [Annotation] -> [Annotation]
makeAttributiveRanges as = ranges
  where ranges = filter isMarkupRangeP as
