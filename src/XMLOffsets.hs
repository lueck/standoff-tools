module XMLOffsets
  ( xmlNode
  ) where

-- A simple xml parser that prints the positions of tags.  Usage:
-- runhaskell xml1.hs < document.xml

import Control.Applicative hiding ((<|>), many)
import Text.Parsec
--import Control.Monad.Identity (Identity)

--testParse :: Stream s m t => Parsec s () a -> s -> Either ParseError a
testParse rule text = parse rule "(source)" text

import LineOffsets as LineOffsets

type AttrName = String
type AttrVal  = String

data Attribute = Attribute (AttrName, AttrVal) deriving (Show)

data XML =  Element String [Attribute] SourcePos SourcePos SourcePos SourcePos [XML]
          | EmptyElement String [Attribute] SourcePos SourcePos
          | Decl String
          | TextNode String
          | SpaceNode
        deriving (Show)

openTag :: Parsec String () String
openTag = do
  char '<'
  name <- many alphaNum
  many space
  char '>'
  return name

--closeTag :: Stream s m Char => String -> ParsecT s u m Char
closeTag name = do
  string "</"
  string name
  many space
  char '>'

elementNode :: Parsec String () XML
elementNode = do
  openStartPos <- getPosition
  name <- openTag
  openEndPos <- getPosition
  content <- many xmlNode
  closeStartPos <- getPosition
  closeTag name
  closeEndPos <- getPosition
  return $ Element name [] openStartPos openEndPos closeStartPos closeEndPos content

emptyElementNode :: Parsec String () XML
emptyElementNode = do
  startPos <- getPosition
  char '<'
  name <- many alphaNum
  many space
  string "/>"
  endPos <- getPosition
  return $ EmptyElement name [] startPos endPos

textNode :: Parsec String () XML
textNode = TextNode <$> many1 (noneOf "<")

xmlNode :: Parsec String () XML
xmlNode = try emptyElementNode <|> try elementNode <|> try textNode

main :: IO ()
main = do
  c <- getContents
  case parse xmlNode "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> print r
