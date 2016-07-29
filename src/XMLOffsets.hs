module XMLOffsets
  ( xmlNode
  ) where

-- A simple xml parser that prints the positions of tags.  Usage:
-- runhaskell xml1.hs < document.xml

import Control.Applicative hiding ((<|>), many)
import Text.Parsec
--import Control.Monad.Identity (Identity)

import LineOffsets

type AttrName = String
type AttrVal  = String

data Attribute = Attribute (AttrName, AttrVal) deriving (Show)

data XML =  Element String [Attribute] Int Int Int Int [XML]
          | EmptyElement String [Attribute] Int Int
          | Decl String
          | TextNode String
          | SpaceNode
        deriving (Show)

openTag :: Parsec String u String
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

elementNode :: Parsec String [Int] XML
elementNode = do
  openStartPos <- offset
  name <- openTag
  openEndPos <- offset
  content <- many xmlNode
  closeStartPos <- offset
  closeTag name
  closeEndPos <- offset
  -- Note: (-1) for the position of tags' ends, because the ending
  -- character was consumed by the parser. Follows, that when ever we
  -- calculate the length of a tag, it is _EndPos - _StartPos + 1
  return $ Element name [] openStartPos (openEndPos - 1) closeStartPos (closeEndPos - 1) content

emptyElementNode :: Parsec String [Int] XML
emptyElementNode = do
  startPos <- offset
  char '<'
  name <- many alphaNum
  many space
  string "/>"
  endPos <- offset
  return $ EmptyElement name [] startPos (endPos - 1)

textNode :: Parsec String [Int] XML
textNode = TextNode <$> many1 (noneOf "<")

xmlNode :: Parsec String [Int] XML
xmlNode = try emptyElementNode <|> try elementNode <|> try textNode


main :: IO ()
main = do
  c <- getContents
  case runParser xmlNode (lineOffsets' c) "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> print r


main' :: IO ()
main' = do
  c <- getContents
  case runParser lineOffsets () "(offsets)" c of
    Left err -> do putStrLn "Error parsing line lengths:"
                   print err
    Right os -> case runParser xmlNode os "(stdin)" c of
                  Left e -> do putStrLn "Error parsing input:"
                               print e
                  Right r -> print r
