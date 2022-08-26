{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module StandOff.XmlParsec
  ( XmlParserState
  , XmlParsec
  , XMLTree'
  , document
  , runXmlParser
  ) where

import Prelude hiding (pi)
import Text.Parsec
import Data.Char (isAlphaNum, isDigit, isAlpha, isSpace)
import qualified Data.Tree.NTree.TypeDefs as NT
import Numeric (readHex, readDec)
import Data.Maybe
import Data.Functor.Identity (Identity)

import StandOff.LineOffsets (Position(..))
import StandOff.DomTypeDefs hiding (char, name)

-- * Combinators

-- | The type for user state of the XML parser.
type XmlParserState = [Int]

-- | A type alias for Parser combinators. This is used for hiding the
-- details. A @XmlParsec s a@ reads an input stream of type s,
-- e.g. 'String' or 'Data.Text'.
--
-- The strings produced by it is '[Char]'. This holds true even if the
-- input stream is of 'Data.Text', because the 'Data.Text' unfolds to
-- a stream of 'Char' as defined by the instance of 'Stream'.
type XmlParsec s a = (Stream s Identity Char) => Parsec s XmlParserState a

-- | An 'XMLTree' parametrized with a types for names and text
-- nodes. This is what this parser produces, regardless of the type
-- fed to the parser. Cf. 'XmlParsec'.
type XMLTree' = XMLTree String String


-- | Get the parsers current position.
getOffset :: Int -> XmlParsec s Position
getOffset cor = do
  offsets <- getState
  pos <- getPosition
  return $ Position { pos_line = sourceLine pos
                    , pos_column = sourceColumn pos
                    -- parsec's column in SourcePos starts with 1, but
                    -- we say that the first char in a file has offset
                    -- of 0. So we decrement the offset by 1.
                    , pos_offset = (offsets !! ((sourceLine pos)-1)) + (sourceColumn pos) + cor - 1
                    }

--name :: (Stream s Identity Char) => Parsec s XmlParserState String
name :: XmlParsec s String
name = do
  c1 <- satisfy nameStartCharP
  cs <- many (satisfy nameCharP)
  return $ [c1] ++ cs

nameStartCharP :: Char -> Bool
nameStartCharP ':' = True
nameStartCharP '_' = True
nameStartCharP c = isAlpha c -- TODO: add other ranges

nameCharP :: Char -> Bool
nameCharP '-' = True
nameCharP '.' = True
nameCharP '\135' = True
nameCharP c = nameStartCharP c || isDigit c

whiteSpaceNode :: XmlParsec s XMLTree'
whiteSpaceNode = do
  startPos <- getOffset 0
  ws <- many1 (satisfy isSpace)
  endPos <- getOffset (-1)
  return $ NT.NTree (TextNode ws startPos endPos) []

openTag :: XmlParsec s (String, [Attribute])
openTag = do
  char '<'
  elName <- name
  attrs <- many $ try attributeNode
  spaces
  char '>'
  return (elName, attrs)

closeTag :: String -> XmlParsec s Char
closeTag elName = do
  string "</"
  string elName
  skipMany space
  char '>'

elementNode :: XmlParsec s XMLTree'
elementNode = do
  openStartPos <- getOffset 0
  nameAndAttrs <- openTag
  openEndPos <- getOffset (-1)
  inner <- many content
  closeStartPos <- getOffset 0
  closeTag (fst nameAndAttrs)
  closeEndPos <- getOffset (-1)
  -- Note: (-1) for the position of tags' ends, because the ending
  -- character was consumed by the parser. Follows, that when ever we
  -- calculate the length of a tag, it is _EndPos - _StartPos + 1
  return $ NT.NTree (Element (fst nameAndAttrs) (snd nameAndAttrs) openStartPos openEndPos closeStartPos closeEndPos) inner

emptyElementNode :: XmlParsec s XMLTree'
emptyElementNode = do
  startPos <- getOffset 0
  char '<'
  elName <- name
  attrs <- many $ try attributeNode
  spaces
  string "/>"
  endPos <- getOffset (-1)
  return $ NT.NTree (EmptyElement elName attrs startPos endPos) []

escape :: XmlParsec s String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"\0\n\r\v\t\b\f"
  return [d, c]

nonEscape :: XmlParsec s Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

quoteChar :: XmlParsec s String
quoteChar = fmap return nonEscape <|> escape <|> (many1 space)

attributeNode :: XmlParsec s Attribute
attributeNode = do
  spaces
  attrName <- many1 (noneOf "= />")
  spaces
  char '='
  spaces
  char '"'
  value <- many $ try quoteChar
  char '"'
  spaces -- Why is this needed?
  return $ Attribute (attrName, (concat value))

textNode :: XmlParsec s XMLTree'
textNode = do
  s <- getOffset 0
  t <- many1 (noneOf "<&")
  e <- getOffset (-1)
  return $ NT.NTree (TextNode t s e) []

comment :: XmlParsec s XMLTree'
comment = do
  startPos <- getOffset 0
  string "<!--"
  c <- manyTill anyChar (try (string "-->"))
  endPos <- getOffset (-1)
  return $ NT.NTree (Comment c startPos endPos) []

cdata :: XmlParsec s XMLTree'
cdata = do
  startPos <- getOffset 0
  string "<![CDATA["
  c <- manyTill anyChar (try (string "]]>"))
  endPos <- getOffset (-1)
  return $ NT.NTree (CData c startPos endPos) []

charRefHex :: XmlParsec s XMLTree'
charRefHex = do
  startPos <- getOffset 0
  string "&#x"
  c <- many1 hexDigit
  char ';'
  endPos <- getOffset (-1)
  i <- case readHex c of
    ((i, ""):[]) -> return i
    _ -> fail $ "Bad character reference &#x" ++ c ++ ";"
  return $ NT.NTree (CharRef i startPos endPos) []

charRefDec :: XmlParsec s XMLTree'
charRefDec = do
  startPos <- getOffset 0
  string "&#"
  c <- many1 digit
  char ';'
  endPos <- getOffset (-1)
  i <- case readDec c of
    ((i, ""):[]) -> return i
    _ -> fail $ "Bad character reference &#" ++ c ++ ";"
  return $ NT.NTree (CharRef i startPos endPos) []

entityRef :: XmlParsec s XMLTree'
entityRef = do
  startPos <- getOffset 0
  char '&'
  c <- name
  char ';'
  endPos <- getOffset (-1)
  return $ NT.NTree (EntityRef c startPos endPos) []


xmlDecl :: XmlParsec s XMLTree'
xmlDecl = do
  s <- getOffset 0
  string "<?xml"
  attrs <- many1 $ try attributeNode
  spaces
  string "?>"
  e <- getOffset (-1)
  return $ NT.NTree (XMLDeclaration attrs s e) []

pi :: XmlParsec s XMLTree'
pi = do
  s <- getOffset 0
  string "<?"
  t <- name
  spaces
  i <- manyTill anyChar (try (string "?>"))
  e <- getOffset (-1)
  return $ NT.NTree (ProcessingInstruction t i s e) []

content :: XmlParsec s XMLTree'
content =
  try emptyElementNode
  <|> try elementNode
  <|> try charRefHex
  <|> try charRefDec
  <|> try entityRef
  <|> try textNode
  <|> try cdata
  <|> try comment
  <|> try pi


prolog :: XmlParsec s [XMLTree']
prolog = do
  -- TODO: What about BOM?
  decl <- optionMaybe (try xmlDecl)
  misc1 <- many misc
  -- TODO
  -- dtype <- dtd
  -- misc2 <- misc
  return $ maybeToList decl ++ misc1

misc :: XmlParsec s XMLTree'
misc =
  try pi
  <|> try whiteSpaceNode
  <|> try comment

-- Parser for XML documents.
document :: XmlParsec s [XMLTree']
document = do
  prlg <- prolog
  tree <- try emptyElementNode <|> try elementNode
  msc <- many misc
  return $ prlg ++ [tree] ++ msc

-- * Parser

-- | Run the parser in a monad. This calls fail on parse errors.
runXmlParser :: (Monad m, Stream s Identity Char) => XmlParserState -> FilePath -> s -> m [XMLTree']
runXmlParser offsets location contents = do
  return $ either (fail . (err++) . show) id $ runParser document offsets location contents
  where
    err = "Error parsing XML input (" ++ location ++ "): "
