module StandOff.External.StandoffModeDump
  where

import Text.Parsec
import qualified Data.Map as Map
import Data.UUID.Types hiding (nil)

import StandOff.AnnotationTypeDefs

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
  return $ MarkupRange { rangeId = rangeId
                       , elementId = elemId
                       , markupType = typ
                       -- Emacs starts with col 1, so -1 for start offset.
                       , startOffset = ((read start)::Int) - 1
                       -- Emacs starts with col 1 and standoff-mode
                       -- defines the ranges end at the following
                       -- char, so -2 for end offset.
                       , endOffset = ((read end)::Int) - 2
                       , text = Nothing -- Just txt -- Drop the text
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
  case (secondUuid) of
    Nothing -> return $ Relation { relationId = Nothing
                                 , subject = firstUuid
                                 , predicate = predicat
                                 , object = obj }
    Just subj -> return $ Relation { relationId = Just firstUuid
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

-- | Run the dump parser in the IO monad.
runELispDumpParser :: SourceName -> String -> IO [Annotation]
runELispDumpParser location contents = do
  return $ either (fail . (err++) . show) id $ parse elDump location contents
  where
    err = "Error parsing ELisp dump file (" ++ location ++ "): "


-- | Parse dumped Elisp annotations.
--
-- Usage:
-- > runhaskell DumpFile.hs < INFILE
main :: IO ()
main = do
  c <- getContents
  rc <- runELispDumpParser "stdin" c
  print c
