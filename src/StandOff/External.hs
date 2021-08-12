module StandOff.External
where

-- | Functions common to external markup.

import qualified Data.Map as Map

type ExternalAttributes = Map.Map String String

-- | External markup should provide a map of features or attributes.
class ToAttributes e where
  attributes :: e -> ExternalAttributes

-- | External markup should provide means for identification.
class IdentifiableSplit e where
  markupId :: e -> Maybe String
  splitNum :: e -> Maybe Int
  updSplitNum :: e -> Int -> e
