{-# LANGUAGE GADTs #-}
module StandOff.External
  ( ToAttributes(..)
  , IdentifiableSplit(..)
  , ExternalMarkup(..)
  , GenericMarkup
  , genMrkp
  )
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


-- * Generic

-- | Generic types for simplifiying apps like the CLI of this package.


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
  -- split typ (MkExternalMarkup e) (x,y) = split typ (genMrkp e) (x,y) -- We cannot do that! :(
  -- We cannot create new GADTs from a GADT, so we need make a new ADT and rewrap it!
  split typ (MkExternalMarkup e) (x,y) =
    (,) <$> MkExternalMarkup . fst <*> MkExternalMarkup . snd $
    split typ (genMrkp e) (x,y)
  splitPoints (MkExternalMarkup e) = splitPoints e


-- ** A generic record

data GenericMarkup
  = GenericMarkup
  { genmrkp_start :: Int
  , genmrkp_end :: Int
  , genmrkp_features :: Map.Map String String
  -- | For internal use:
  , genmrkp_splitNum :: Maybe Int  -- ^ number of the split
  } deriving (Show)


instance TextRange GenericMarkup where
  start = genmrkp_start
  end = genmrkp_end
  split _ range (e1, s2) = (range {genmrkp_end = e1}, range {genmrkp_start = s2})

instance ToAttributes GenericMarkup where
  attributes = genmrkp_features

instance IdentifiableSplit GenericMarkup where
  markupId = Map.lookup "id" . genmrkp_features
  splitNum = genmrkp_splitNum
  updSplitNum r i = r { genmrkp_splitNum = Just i }


genMrkp :: (TextRange a, ToAttributes a, IdentifiableSplit a) => a -> GenericMarkup
genMrkp = GenericMarkup
  <$> start
  <*> end
  <*> attributes
  <*> splitNum
