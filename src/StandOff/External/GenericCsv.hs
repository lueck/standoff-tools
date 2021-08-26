{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module StandOff.External.GenericCsv
  ( GenericCsvMarkup(..)
  , parseCsv
  , runCsvParser
  , startEndMarkup
  , lineColumnMarkup
  , lineColumnLengthMarkup
  )
where

-- | This module provides a generic parser for external markup
-- represented as CSV.


import qualified Data.Map as Map
import qualified Data.Csv as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.IO
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad

import StandOff.TextRange
import StandOff.External
import StandOff.LineOffsets (offset)

-- * Type definitions

data GenericCsvMarkup
  = NamedGenericCsvMarkup
  { ncsv_start :: Int
  , ncsv_end :: Int
  , ncsv_features :: Map.Map String String
  -- | For internal use:
  , ncsv_splitNum :: Maybe Int  -- ^ number of the split
  } deriving (Show)


instance TextRange GenericCsvMarkup where
  start = ncsv_start
  end = ncsv_end
  split _ range (e1, s2) = (range {ncsv_end = e1}, range {ncsv_start = s2})

instance ToAttributes GenericCsvMarkup where
  attributes = ncsv_features

instance IdentifiableSplit GenericCsvMarkup where
  markupId = Map.lookup "id" . ncsv_features
  splitNum = ncsv_splitNum
  updSplitNum r i = r { ncsv_splitNum = Just i }


-- * Parse CSV

-- | Parse CSV input from IO 'Handle'. The first two arguments are
-- passed to 'parseCsv'. See there for details.
runCsvParser :: (Map.Map String String -> Maybe GenericCsvMarkup)
             -> (BS.ByteString -> T.Text)
             -> Handle                                            -- ^ IO handle
             -> IO [GenericCsvMarkup]
runCsvParser mkMarkup dec h = do
  c <- BL.hGetContents h
  return $ either fail id $ parseCsv mkMarkup dec c

-- | Parse CSV input from lazy bytestring. Use e.g. 'decodeUtf8' for
-- decoding the character encoding.
parseCsv :: (Map.Map String String -> Maybe GenericCsvMarkup)
         -- ^ function for making 'GenericCsvMarkup' from parsed
         -- values. The mapping that the function takes as input maps
         -- column names to a single row of values. If no
         -- 'GenericCsvMarkup' can be created from this mapping, the
         -- function should return Nothing. (Nothing values will be
         -- removed silently from the list of external markup.)
         -> (BS.ByteString -> T.Text)
         -- ^ decoding input
         -> BL.ByteString
         -- ^ the csv input string
         -> Either String [GenericCsvMarkup]
parseCsv mkMarkup dec s =
  -- first line is taken as column names
  -- we zip the columns of the other lines with these names
  -- and generate a 'Map' for each line
  fmap (\vec -> catMaybes $
         map mkMarkup $
         map (Map.fromList .
              (zip (map (T.unpack . dec . BL.toStrict) $ V.toList $ V.head vec)) .
              (map (T.unpack . dec . BL.toStrict) . V.toList)) $
         tail $
         V.toList vec
       ) markup
  where
    markup :: Either String (Vector (Vector BL.ByteString))
    -- Note: decodeByNameWith does not work with (Vector Bytestring), does it?
    markup = C.decodeWith C.defaultDecodeOptions C.NoHeader s

-- | Make markup from CSV with *start character offset and end
-- character offset*. "start" and "end" must both be present in the
-- CSV header und the values must be integrals. If there is not both
-- of them the line will be 'Nothing'. This function can serve as an
-- argument to 'runCsvParser' and 'parseCsv'.
startEndMarkup :: Map.Map String String -> Maybe GenericCsvMarkup
startEndMarkup line = NamedGenericCsvMarkup
  <$> (join $ fmap readMaybe $ Map.lookup "start" line)
  <*> (join $ fmap readMaybe $ Map.lookup "end" line)
  <*> Just line
  <*> Just Nothing

-- | Make markup from CSV with the start and end expressed by tuples
-- of line and column. "startline", "startcolumn", "endline", and
-- "endcolumn" must be present in the CSV header and the values must
-- be integrals.
lineColumnMarkup :: [Int] -> Map.Map String String -> Maybe GenericCsvMarkup
lineColumnMarkup offsets line = NamedGenericCsvMarkup
  <$> start'
  <*> end'
  <*> Just line
  <*> Just Nothing
  where
    start' = join $ offset offsets
      <$> (join $ fmap readMaybe $ Map.lookup "startline" line)
      <*> (join $ fmap readMaybe $ Map.lookup "startcolumn" line)
    end' = join $ offset offsets
      <$> (join $ fmap readMaybe $ Map.lookup "endline" line)
      <*> (join $ fmap readMaybe $ Map.lookup "endcolumn" line)

-- | Make markup from CSV with the start expressed by a tuple of line
-- and column and the length of the range given. "line", "column", and
-- "length" must be present in the CSV header and the values must be
-- integrals.
lineColumnLengthMarkup :: [Int] -> Map.Map String String -> Maybe GenericCsvMarkup
lineColumnLengthMarkup offsets line = NamedGenericCsvMarkup
  <$> start'
  <*> ((+) <$> start' <*> (join $ fmap readMaybe $ Map.lookup "length" line))
  <*> Just line
  <*> Just Nothing
  where
    start' = join $ offset offsets
      <$> (join $ fmap readMaybe $ Map.lookup "line" line)
      <*> (join $ fmap readMaybe $ Map.lookup "column" line)
