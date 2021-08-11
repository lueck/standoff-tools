{-# LANGUAGE OverloadedStrings, DeriveGeneric, TupleSections #-}

module StandOff.External.StandoffModeDump
  ( StandoffModeRange(..)
  , StandoffModeAnnotations(..)
  , runJsonParser
  , elDump
  , runELispDumpParser
  )
  where

-- | This module provides functions for parsing external annotations
-- that where generated in Emacs' standoff-mode.

import GHC.Generics
import Data.Time
import Data.UUID.Types hiding (nil)
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as A
import Control.Applicative ((<|>))
import Text.Read (readMaybe)
import Text.Parsec hiding ((<|>))
import qualified Data.ByteString.Lazy as BS
import System.IO
import qualified Data.Map as Map
import Data.Maybe

import StandOff.TextRange
import StandOff.External


-- * Data type

-- | A record for markup ranges from Emacs' standoff-mode.
data StandoffModeRange
  = StandoffModeRange             -- ^ range of characters in source file
    { somr_id :: Maybe UUID       -- ^ the UUID of the range
    , somr_elementId :: UUID      -- ^ the UUID of the element to which the range belongs
    , somr_type :: String         -- ^ the annotation type
    , somr_start :: Int           -- ^ the start character offset
    , somr_end :: Int             -- ^ the end character offset
    , somr_createdAt :: Maybe UTCTime -- ^ a unix timestamp
    , somr_createdBy :: Maybe String -- ^ the annotator's user name
    }
  deriving (Show)

instance TextRange (StandoffModeRange) where
  start = somr_start
  end = somr_end
  split _ range (e1, s2) = (range {somr_end = e1}, range {somr_start = s2})

instance ToAttributes StandoffModeRange where
  attributes r = Map.fromList $ catMaybes
    [ Just ("tagger", "standoff-mode")
    , (fmap (("rangeId",) . show ) $ somr_id r)
    , Just ("elementId", (show $ somr_elementId r))
    , Just ("type", somr_type r)
    , (fmap (("createdAt",) . show) $ somr_createdAt r)
    , (fmap ("createdBy",) $ somr_createdBy r)
    ]


-- | A record for representing the contents of standoff-mode's
-- external markup. Note: This is incomplete, since we parse
-- interesting stuff only.
data StandoffModeAnnotations
  = StandoffModeAnnotations
  { som_md5sum :: String
  , som_ranges :: [StandoffModeRange]
  }
  deriving (Show, Generic)


-- * Parsing JSON files

instance A.FromJSON StandoffModeRange where
  parseJSON (A.Object v) = StandoffModeRange
    <$> v .: "markupRangeId"
    <*> v .: "markupElementId"
    <*> v .: "qualifiedName"
    <*> (fmap (\x -> x - 1) $ ((v .: "sourceStart") <|> (fmap (assertInt . readMaybe) $ v .: "sourceStart")))
    <*> (fmap (\x -> x - 2) $ ((v .: "sourceEnd") <|> (fmap (assertInt . readMaybe) $ v .: "sourceEnd")))
    <*> (fmap readFormattedTime $ v .: "createdAt")
    <*> v .: "createdBy"
    where
      assertInt Nothing = 0
      assertInt (Just i) = i
  parseJSON invalid = A.typeMismatch "Object" invalid

-- | Parse the formatted time in "createdAt"
readFormattedTime :: String -> Maybe UTCTime
readFormattedTime s = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" s


instance A.FromJSON StandoffModeAnnotations where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = somJsonFields }
    where
      somJsonFields :: String -> String
      somJsonFields "som_md5sum" = "md5sum"
      somJsonFields "som_ranges" = "MarkupRanges"
      somJsonFields s = s

-- | Parse all 'StandoffModeRange' objects found in IO 'Handle'.
runJsonParser :: Handle -> IO [StandoffModeRange]
runJsonParser h = do
  c <- BS.hGetContents h
  let ann = A.decode c :: Maybe StandoffModeAnnotations
  -- TODO: Wrap the result in GADT.
  return $ maybe [] id $ fmap som_ranges ann


-- * Parsing elisp dump files


uuid :: Parsec String () UUID
uuid = do
  char '"'
  hyphened <- count 36 $ oneOf $ '-':['a'..'h']++['0'..'9']
  char '"'
  case fromString(hyphened) of
    Just valid -> return valid
    Nothing -> error $ "Not a valid UUID: " ++ hyphened

timeStamp :: Parsec String () String
timeStamp = do
  char '('
  t <- manyTill (digit <|> space) $ char ')'
  return t

escape :: Parsec String () String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"\0\n\r\v\t\b\f"
  return [d, c]

nonEscape :: Parsec String () Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

quoteChar :: Parsec String () String
quoteChar = fmap return nonEscape <|> escape <|> (many1 space)

quoteString :: Parsec String () String
quoteString = do
  char '"'
  s <- many $ try quoteChar
  char '"'
  return $ concat s

nil :: Parsec String () [StandoffModeRange]
nil = do
  spaces
  string "nil"
  spaces
  return []

markupRange :: Parsec String () StandoffModeRange
markupRange = do
  char '('
  spaces
  elemId <- uuid
  spaces
  rangeId <- optionMaybe $ try uuid
  spaces
  typ <- quoteString
  spaces
  start <- manyTill digit space
  end <- manyTill digit space
  txt <- quoteString
  spaces
  optional $ try timeStamp
  spaces
  optional $ try quoteString
  spaces
  char ')'
  spaces
  return $ StandoffModeRange
    { somr_id = rangeId
    , somr_elementId = elemId
    , somr_type = typ
    -- Emacs starts with col 1, so -1 for start offset.
    , somr_start = ((read start)::Int) - 1
    -- Emacs starts with col 1 and standoff-mode
    -- defines the ranges end at the following
    -- char, so -2 for end offset.
    , somr_end = ((read end)::Int) - 2
    , somr_createdBy = Nothing -- FIXME
    , somr_createdAt = Nothing -- FIXME
    }

markupRanges :: Parsec String () [StandoffModeRange]
markupRanges = do
  spaces
  char '('
  spaces
  rs <- many markupRange
  spaces
  char ')'
  return rs

relation :: Parsec String () ()
relation = do
  char '('
  spaces
  firstUuid <- uuid
  spaces
  secondUuid <- optionMaybe $ try uuid
  spaces
  predicat <- quoteString
  spaces
  obj <- uuid
  spaces
  optional $ try timeStamp
  spaces
  optional $ try quoteString
  spaces
  char ')'
  spaces
  return ()

relations :: Parsec String () [StandoffModeRange]
relations = do
  spaces
  char '('
  rels <- many relation
  char ')'
  spaces
  return []

elDump :: Parsec String () [StandoffModeRange]
elDump = do
  spaces
  string "(setq standoff-markup-read-function-dumped (quote"
  ranges <- try markupRanges <|> try nil
  string "))"
  spaces
  string "(setq standoff-relations-read-function-dumped (quote"
  rels <- try relations <|> try nil
  string "))"
  -- FIXME: parse literal predicates etc., too.
  skipMany anyChar
  return $ ranges++rels

-- | Run the dump parser in the IO monad.
runELispDumpParser :: Handle -> IO [StandoffModeRange]
runELispDumpParser h = do
  contents <- hGetContents h
  return $ either (fail . (err++) . show) id $ parse elDump (show h) contents
  where
    err = "Error parsing ELisp dump (" ++ (show h) ++ "): "


-- | Parse annotations.
--
-- Usage:
-- > runhaskell DumpFile.hs < INFILE
main :: IO ()
main = do
  rc <- runELispDumpParser stdin
  -- c <- BS.getContents
  -- let rc = A.decode c :: Maybe StandoffModeAnnotations
  print rc
