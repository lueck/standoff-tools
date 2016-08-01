module DumpElParser
  where

import Text.Parsec

import AnnotationData

uuid :: Parsec String () String
uuid = do
  char '"'
  p1 <- count 8 $ oneOf $ ['a'..'h']++['0'..'9']
  char '-'
  p2 <- count 4 $ oneOf $ ['a'..'h']++['0'..'9']
  char '-'
  p3 <- count 4 $ oneOf $ ['a'..'h']++['0'..'9']
  char '-'
  p4 <- count 4 $ oneOf $ ['a'..'h']++['0'..'9']
  char '-'
  p5 <- count 12 $ oneOf $ ['a'..'h']++['0'..'9']
  char '"'
  return $ p1++p2++p3++p4++p5

timeStamp :: Parsec String () String
timeStamp = do
  char '('
  t <- manyTill (digit <|> space) $ char ')'
  return t

quoteString :: Parsec String () String
quoteString = do
  char '"'
  txt <- manyTill anyChar $ char '"'
  return txt

markupRange :: Parsec String () Annotation
markupRange = do
  char '('
  spaces
  id' <- uuid
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
  return $ MarkupRange { rangeId = ""
                       , elementId = id'
                       , markupType = typ
                       , startOffset = (read start)::Int
                       , endOffset = (read end)::Int
                       , text = txt }

relation :: Parsec String () Annotation
relation = do
  char '('
  spaces
  subj <- uuid
  spaces
  pred <- quoteString
  spaces
  obj <- uuid
  spaces
  optional $ try timeStamp
  spaces
  optional $ try quoteString
  spaces
  char ')'
  spaces
  return $ Relation { subject = subj
                    , predicate = pred
                    , object = obj }


elDump :: Parsec String () [Annotation]
elDump = do
  spaces
  string "(setq standoff-markup-read-function-dumped (quote ("
  ranges <- manyTill markupRange $ string ")))"
  -- FIXME: parse relations etc., too.
  spaces
  string "(setq standoff-relations-read-function-dumped (quote ("
  relations <- manyTill relation $ string ")))"
  skipMany anyChar
  return $ ranges++relations


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
