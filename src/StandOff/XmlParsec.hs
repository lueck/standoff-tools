module StandOff.XmlParsec
  ( xmlDocument
  , runXmlParser
  ) where

import Text.Parsec
import Data.Char (isAlphaNum)
import qualified Data.Tree.NTree.TypeDefs as NT

import StandOff.LineOffsets
import StandOff.DomTypeDefs

-- | An 'XMLTree' parametrized with a types for names and text
-- nodes. This is what this parser produces.
type XMLTree' = XMLTree String String

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

elementNode :: Parsec String [Int] XMLTree'
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

emptyElementNode :: Parsec String [Int] XMLTree'
emptyElementNode = do
  startPos <- getOffset 0
  char '<'
  elName <- many1 alphaNum
  attrs <- many $ try attributeNode
  spaces
  string "/>"
  endPos <- getOffset (-1)
  return $ NT.NTree (EmptyElement elName attrs startPos endPos) []

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

textNode :: Parsec String [Int] XMLTree'
textNode = do
  s <- getOffset 0
  t <- many1 (noneOf "<")
  e <- getOffset 0
  return $ NT.NTree (TextNode t s e) []

comment :: Parsec String [Int] XMLTree'
comment = do
  startPos <- getOffset 0
  string "<!--"
  c <- manyTill anyChar (try (string "-->"))
  endPos <- getOffset (-1)
  return $ NT.NTree (Comment ("<!--"++c++"-->") startPos endPos) []

xmlDecl :: Parsec String [Int] XMLTree'
xmlDecl = do
  s <- getOffset 0
  string "<?xml"
  attrs <- many1 $ try attributeNode
  spaces
  string "?>"
  e <- getOffset (-1)
  return $ NT.NTree (XMLDeclaration attrs s e) []

processingInstruction :: Parsec String [Int] XMLTree'
processingInstruction = do
  s <- getOffset 0
  string "<?"
  t <- many1 $ satisfy isTagNameCharP
  attrs <- many1 $ try attributeNode
  spaces
  string "?>"
  e <- getOffset (-1)
  return $ NT.NTree (ProcessingInstruction t attrs s e) []

processingInstructionMaybeSpace :: Parsec String [Int] XMLTree'
processingInstructionMaybeSpace = do
  p <- processingInstruction
  spaces
  return p

xmlNode :: Parsec String [Int] XMLTree'
xmlNode = try emptyElementNode <|> try elementNode <|> try textNode <|> try comment

-- Parser for XML documents.
-- FIXME: Arbitrary many comments are allowed beside the root element.
xmlDocument :: Parsec String [Int] [XMLTree']
xmlDocument = do
  skipMany space
  decl <- optionMaybe $ try xmlDecl
  skipMany space
  pInstr <- many $ try processingInstructionMaybeSpace
  skipMany space
  tree <- try emptyElementNode <|> try elementNode
  skipMany space
  return $ case decl of
             Just d@(NT.NTree (XMLDeclaration _ _ _) []) -> [d]++pInstr++[tree]
             _ -> pInstr++[tree]

-- | Run the parser in the IO monad.
runXmlParser :: [Int] -> FilePath -> String -> IO [XMLTree']
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
