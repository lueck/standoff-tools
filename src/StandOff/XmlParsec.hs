module StandOff.XmlParsec
  ( xmlDocument
  , runXmlParser
  ) where

import Text.Parsec
import Data.Char (isAlphaNum)

import StandOff.LineOffsets
import StandOff.DomTypeDefs

isTagNameCharP :: Char -> Bool
isTagNameCharP '-' = True
isTagNameCharP c = isAlphaNum c

openTag :: Parsec String [Int] (String, [Attribute])
openTag = do
  char '<'
  elName <- many1 alphaNum
  attrs <- many $ try attributeNode
  spaces
  char '>'
  return (elName, attrs)

closeTag :: String -> Parsec String [Int] Char
closeTag elName = do
  string "</"
  string elName
  skipMany space
  char '>'

elementNode :: Parsec String [Int] XML
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
  return $ Element (fst nameAndAttrs) (snd nameAndAttrs) openStartPos openEndPos closeStartPos closeEndPos inner

emptyElementNode :: Parsec String [Int] XML
emptyElementNode = do
  startPos <- getOffset 0
  char '<'
  elName <- many1 alphaNum
  attrs <- many $ try attributeNode
  spaces
  string "/>"
  endPos <- getOffset (-1)
  return $ EmptyElement elName attrs startPos endPos

escape :: Parsec String [Int] String
escape = do
  d <- char '\\'
  c <- oneOf "\\\"\0\n\r\v\t\b\f"
  return [d, c]

nonEscape :: Parsec String [Int] Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

quoteChar :: Parsec String [Int] String
quoteChar = fmap return nonEscape <|> escape <|> (many1 space)

attributeNode :: Parsec String [Int] Attribute
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

textNode :: Parsec String [Int] XML
textNode = do
  s <- getOffset 0
  t <- many1 (noneOf "<")
  e <- getOffset 0
  return $ TextNode t s e

comment :: Parsec String [Int] XML
comment = do
  startPos <- getOffset 0
  string "<!--"
  c <- manyTill anyChar (try (string "-->"))
  endPos <- getOffset (-1)
  return $ Comment ("<!--"++c++"-->") startPos endPos

xmlDecl :: Parsec String [Int] XML
xmlDecl = do
  s <- getOffset 0
  string "<?xml"
  attrs <- many1 $ try attributeNode
  spaces
  string "?>"
  e <- getOffset (-1)
  return $ XMLDeclaration attrs s e

processingInstruction :: Parsec String [Int] XML
processingInstruction = do
  s <- getOffset 0
  string "<?"
  t <- many1 $ satisfy isTagNameCharP
  attrs <- many1 $ try attributeNode
  spaces
  string "?>"
  e <- getOffset (-1)
  return $ ProcessingInstruction t attrs s e

processingInstructionMaybeSpace :: Parsec String [Int] XML
processingInstructionMaybeSpace = do
  p <- processingInstruction
  spaces
  return p

xmlNode :: Parsec String [Int] XML
xmlNode = try emptyElementNode <|> try elementNode <|> try textNode <|> try comment

-- Parser for XML documents.
-- FIXME: Arbitrary many comments are allowed beside the root element.
xmlDocument :: Parsec String [Int] [XML]
xmlDocument = do
  skipMany space
  decl <- optionMaybe $ try xmlDecl
  skipMany space
  pInstr <- many $ try processingInstructionMaybeSpace
  skipMany space
  tree <- try emptyElementNode <|> try elementNode
  skipMany space
  return $ case decl of
             Nothing -> pInstr++[tree]
             Just (XMLDeclaration attrs s e) -> [(XMLDeclaration attrs s e)]++pInstr++[tree]

-- | Run the parser in the IO monad.
runXmlParser :: [Int] -> FilePath -> String -> IO [XML]
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
