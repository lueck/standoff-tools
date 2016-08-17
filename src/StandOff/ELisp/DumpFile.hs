{-# LANGUAGE OverloadedStrings #-}
module StandOff.ELisp.DumpFile
  where

import Text.Parsec
import qualified Data.Map as Map
import Data.UUID (UUID, fromString)
import Data.UUID.V4 (nextRandom)

import StandOff.Data.Annotation

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

nil :: Parsec String () [Annotation]
nil = do
  spaces
  string "nil"
  spaces
  return []

markupRange :: Parsec String () Annotation
markupRange = do
  char '('
  spaces
  elemId <- uuid
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
  return $ MarkupRange { rangeId = Nothing
                       , elementId = elemId
                       , markupType = typ
                       , startOffset = (read start)::Int
                       , endOffset = (read end)::Int
                       , text = txt
                       , attributes = Map.empty }

markupRanges :: Parsec String () [Annotation]
markupRanges = do
  spaces
  char '('
  spaces
  rs <- many markupRange
  spaces
  char ')'
  return rs

relation :: Parsec String () Annotation
relation = do
  char '('
  spaces
  relId <- uuid
  spaces
  subj <- uuid
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
  return $ Relation { relationId = relId
                    , subject = subj
                    , predicate = predicat
                    , object = obj }

relations :: Parsec String () [Annotation]
relations = do
  spaces
  char '('
  rels <- many relation
  char ')'
  spaces
  return rels

elDump :: Parsec String () [Annotation]
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


parseDumpString :: String -> String -> Either ParseError [Annotation]
parseDumpString doc fname =
  runParser elDump () fname doc

main :: IO ()
main = do
  c <- getContents
  case parse elDump "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> print r
