{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module StandOff.External.GenericCsv
  ( GenericCsvMarkup(..)
  , parseCsv
  , runCsvParser
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
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Data.Maybe
import Text.Read (readMaybe)
import Control.Monad

import StandOff.TextRange
import StandOff.External


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

-- | Parse CSV input from IO 'Handle'.
runCsvParser :: (BS.ByteString -> T.Text) -> Handle -> IO [GenericCsvMarkup]
runCsvParser dec h = do
  c <- BL.hGetContents h
  return $ either fail id $ parseCsv dec c

-- | Parse CSV input from lazy bytestring. Use e.g. 'decodeUtf8' for
-- decoding the character encoding.
parseCsv :: (BS.ByteString -> T.Text) -> BL.ByteString -> Either String [GenericCsvMarkup]
parseCsv dec s =
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

-- | If there is not both, an integral start and end, the line will be
-- 'Nothing'.
mkMarkup :: Map.Map String String -> Maybe GenericCsvMarkup
mkMarkup line = NamedGenericCsvMarkup
  <$> (join $ fmap readMaybe $ Map.lookup "start" line)
  <*> (join $ fmap readMaybe $ Map.lookup "end" line)
  <*> Just line
  <*> Just Nothing
