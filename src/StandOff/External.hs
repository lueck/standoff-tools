{-# LANGUAGE GADTs #-}
module StandOff.External
where

-- | Functions common to external markup.

import qualified Data.Map as Map

import StandOff.TextRange


type ExternalAttributes = Map.Map String String

-- | External markup should provide a map of features or attributes.
class ToAttributes e where
  attributes :: e -> ExternalAttributes

-- | External markup should provide means for identification.
class IdentifiableSplit e where
  markupId :: e -> Maybe String
  splitNum :: e -> Maybe Int
  updSplitNum :: e -> Int -> e


-- | A GADT that can be wrapped around types of external markup which
-- is an instance of 'TextRange', 'ToAttributes', and
-- 'IdentifiableSplit'.
data ExternalMarkup
  where
    MkExternalMarkup :: (TextRange a, ToAttributes a, IdentifiableSplit a) => a -> ExternalMarkup

instance TextRange ExternalMarkup where
  start (MkExternalMarkup e) = start e
  end (MkExternalMarkup e) = end e
  spans (MkExternalMarkup e) = spans e
  split typ (MkExternalMarkup e) (x,y) = split typ e (x,y) -- > We cannot do that! :(
  splitPoints (MkExternalMarkup e) = splitPoints e
