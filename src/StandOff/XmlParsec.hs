module StandOff.XmlParsec
  ( xmlDocument
  , runXmlParser
  ) where

import Text.Parsec
import Data.Char (isAlphaNum, isDigit, isAlpha, isSpace)
import qualified Data.Tree.NTree.TypeDefs as NT
import Numeric (readHex, readDec)
import Data.Maybe

import StandOff.LineOffsets
import StandOff.DomTypeDefs hiding (char, name)

-- | An 'XMLTree' parametrized with a types for names and text
-- nodes. This is what this parser produces.
type XMLTree' = XMLTree String String

-- | The type for user state of the XML parser.
type XmlParserState = [Int]


name :: Parsec String XmlParserState String
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

isTagNameCharP :: Char -> Bool
isTagNameCharP '-' = True
isTagNameCharP c = isAlphaNum c

whiteSpaceNode :: Parsec String XmlParserState XMLTree'
whiteSpaceNode = do
  startPos <- getOffset 0
  ws <- many1 (satisfy isSpace)
  endPos <- getOffset 0
  return $ NT.NTree (TextNode ws startPos endPos) []

openTag :: Parsec String XmlParserState (String, [Attribute])
openTag = do
  char '<'
  elName <- many1 alphaNum
  attrs <- many $ try attributeNode
  spaces
  char '>'
  return (elName, attrs)

closeTag :: String -> Parsec String XmlParserState Char
closeTag elName = do
  string "</"
  string elName
  skipMany space
  char '>'

elementNode :: Parsec String XmlParserState XMLTree'
elementNode = do
  openStartPos <- getOffset 0
  nameAndAttrs <- openTag
  openEndPos <- getOffset (-1)
  inner <- many xmlNode
  closeStartPos <- getOffset 0
  closeTag (fst nameAndAttrs)
  closeEndPos <- getOffset (-1)
  -- Note: (-1) for the position of tags' ends, because the ending
  -- character was consumed by the parser. Follows, that when ever we
  -- calculate the length of a tag, it is _EndPos - _StartPos + 1
  return $ NT.NTree (Element (fst nameAndAttrs) (snd nameAndAttrs) openStartPos openEndPos closeStartPos closeEndPos) inner

emptyElementNode :: Parsec String XmlParserState XMLTree'
emptyElementNode = do
  startPos <- getOffset 0
  char '<'
  elName <- many1 alphaNum
  attrs <- many $ try attributeNode
  spaces
  string "/>"
  endPos <- getOffset (-1)
  return $ NT.NTree (EmptyElement elName attrs startPos endPos) []

escape :: Parsec String XmlParserState String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"\0\n\r\v\t\b\f"
  return [d, c]

nonEscape :: Parsec String XmlParserState Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

quoteChar :: Parsec String XmlParserState String
quoteChar = fmap return nonEscape <|> escape <|> (many1 space)

attributeNode :: Parsec String XmlParserState Attribute
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

textNode :: Parsec String XmlParserState XMLTree'
textNode = do
  s <- getOffset 0
  t <- many1 (noneOf "<&")
  e <- getOffset (-1)
  return $ NT.NTree (TextNode t s e) []

comment :: Parsec String XmlParserState XMLTree'
comment = do
  startPos <- getOffset 0
  string "<!--"
  c <- manyTill anyChar (try (string "-->"))
  endPos <- getOffset (-1)
  return $ NT.NTree (Comment ("<!--"++c++"-->") startPos endPos) []

cdata :: Parsec String XmlParserState XMLTree'
cdata = do
  startPos <- getOffset 0
  string "<![CDATA["
  c <- manyTill anyChar (try (string "]]>"))
  endPos <- getOffset (-3)
  return $ NT.NTree (CData c startPos endPos) []

charRefHex :: Parsec String XmlParserState XMLTree'
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

charRefDec :: Parsec String XmlParserState XMLTree'
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

entityRef :: Parsec String XmlParserState XMLTree'
entityRef = do
  startPos <- getOffset 0
  char '&'
  c <- name
  char ';'
  endPos <- getOffset (-1)
  return $ NT.NTree (EntityRef c startPos endPos) []


xmlDecl :: Parsec String XmlParserState XMLTree'
xmlDecl = do
  s <- getOffset 0
  string "<?xml"
  attrs <- many1 $ try attributeNode
  spaces
  string "?>"
  e <- getOffset (-1)
  return $ NT.NTree (XMLDeclaration attrs s e) []

processingInstruction :: Parsec String XmlParserState XMLTree'
processingInstruction = do
  s <- getOffset 0
  string "<?"
  t <- many1 $ satisfy isTagNameCharP
  attrs <- many1 $ try attributeNode
  spaces
  string "?>"
  e <- getOffset (-1)
  return $ NT.NTree (ProcessingInstruction t attrs s e) []

processingInstructionMaybeSpace :: Parsec String XmlParserState XMLTree'
processingInstructionMaybeSpace = do
  p <- processingInstruction
  spaces
  return p

xmlNode :: Parsec String XmlParserState XMLTree'
xmlNode =
  try emptyElementNode
  <|> try elementNode
  <|> try charRefHex
  <|> try charRefDec
  <|> try entityRef
  <|> try textNode
  <|> try cdata
  <|> try comment


prolog :: Parsec String XmlParserState [XMLTree']
prolog = do
  decl <- optionMaybe (try xmlDecl)
  misc1 <- many misc
  -- dtype <- dtd
  -- misc2 <- misc
  return $ maybeToList decl ++ misc1

misc :: Parsec String XmlParserState XMLTree'
misc =
  try processingInstruction
  <|> try whiteSpaceNode
  <|> try comment

-- Parser for XML documents.
xmlDocument :: Parsec String XmlParserState [XMLTree']
xmlDocument = do
  prlg <- prolog
  tree <- try emptyElementNode <|> try elementNode
  msc <- many misc
  return $ prlg ++ [tree] ++ msc

-- | Run the parser in the IO monad.
runXmlParser :: XmlParserState -> FilePath -> String -> IO [XMLTree']
runXmlParser offsets location contents = do
  return $ either (fail . (err++) . show) id $ runParser xmlDocument offsets location contents
  where
    err = "Error parsing XML input (" ++ location ++ "): "


-- | A simple xml parser that prints the positions of tags.
--
-- Usage:
-- > runhaskell XMLOffsets.hs < document.xml
main :: IO ()
main = do
  c <- getContents
  offsets <- runLineOffsetParser "(stdin)" c
  rc <- runXmlParser offsets "(stdin)" c
  print rc
