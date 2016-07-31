module XMLOffsets
  ( parseString
  , parseFile
  , xmlDocument
  , XML
  , elementName
  , elementAttributes
  , elementOpenTagPosition
  , elementCloseTagPosition
  , elementContent
  , textContent
  , xmlSpanning
  ) where

-- A simple xml parser that prints the positions of tags.  Usage:
-- runhaskell xml1.hs < document.xml

--import Control.Applicative hiding ((<|>), many)
import Text.Parsec
--import Control.Monad.Identity (Identity)

import LineOffsets

type AttrName = String
type AttrVal  = String

data Attribute = Attribute (AttrName, AttrVal) deriving (Show)

data XML =  Element { name :: String
                    , attributes ::  [Attribute]
                    , startOpenTag :: Position
                    , endOpenTag :: Position
                    , startCloseTag :: Position
                    , endCloseTag :: Position
                    , content :: [XML] }
          | EmptyElement { name :: String
                         , attributes :: [Attribute]
                         , startTag :: Position
                         , endTag :: Position }
          | XMLDecl { declaration :: String
                 , start :: Position
                 , end :: Position }
          | TextNode { text :: String
                     , start :: Position
                     , end :: Position }
          | Comment { text :: String
                    , start :: Position
                    , end :: Position }
          | SpaceNode
        deriving (Show)

elementName :: XML -> String
elementName (Element n _ _ _ _ _ _) = n
elementName (EmptyElement n _ _ _) = n

elementAttributes :: XML -> [Attribute]
elementAttributes (Element _ attrs _ _ _ _ _) = attrs
elementAttributes (EmptyElement _ attrs _ _) = attrs

xmlSpanning :: XML -> (Position, Position)
xmlSpanning (Element _ _ s _ _ e _) = (s, e)
xmlSpanning (EmptyElement _ _ s e) = (s, e)
xmlSpanning (TextNode _ s e) = (s, e)
xmlSpanning (Comment _ s e) = (s, e)
xmlSpanning (XMLDecl _ s e) = (s, e)

elementOpenTagPosition :: XML -> (Position, Position)
elementOpenTagPosition (Element _ _ s e _ _ _) = (s, e)
elementOpenTagPosition (EmptyElement _ _ s e) = (s, e)

elementCloseTagPosition :: XML -> (Position, Position)
elementCloseTagPosition (Element _ _ _ _ s e _) = (s, e)
elementCloseTagPosition (EmptyElement _ _ s e) = (s, e)

elementContent :: XML -> [XML]
elementContent (Element _ _ _ _ _ _ c) = c

textContent :: XML -> String
textContent (TextNode t _ _) = t


openTag :: Parsec String [Int] (String, [Attribute])
openTag = do
  char '<'
  elName <- many1 alphaNum
  attrs <- many attributeNode
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
  attrs <- many attributeNode
  spaces
  string "/>"
  endPos <- getOffset (-1)
  return $ EmptyElement elName attrs startPos endPos

attributeNode :: Parsec String [Int] Attribute
attributeNode = do
  spaces
  attrName <- many1 (noneOf "= />")  
  spaces
  char '='
  spaces
  char '"'
  value <- many1 (noneOf ['"'])
  char '"'
  spaces -- Why is this needed?
  return $ Attribute (attrName, value)

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

-- XML declaration eg. <?xml version="1.0" encoding="UTF-8"?>
xmlDecl :: Parsec String [Int] XML
xmlDecl = do
  s <- getOffset 0
  string "<?xml"
  decl <- many (noneOf "?>")
  string "?>"
  e <- getOffset 0
  return $ XMLDecl decl s e

xmlNode :: Parsec String [Int] XML
xmlNode = try emptyElementNode <|> try elementNode <|> try textNode <|> try comment

xmlDocument :: Parsec String [Int] XML
xmlDocument = do
  skipMany space
  --try xmlDecl -- FIXME: parse optional xml declaration
  skipMany space
  tree <- try emptyElementNode <|> try elementNode
  skipMany space
  return $ tree


parseString :: String -> String -> Either ParseError XML
parseString doc fname =
  runParser xmlDocument (lineOffsets' doc) fname doc 


parseFile :: FilePath -> IO (Either ParseError XML)
parseFile fname = do
  c <- readFile fname
  return $ runParser xmlDocument (lineOffsets' c) fname c


main :: IO ()
main = do
  c <- getContents
  case parseString c "(stdin)" of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> print r


main'' :: IO ()
main'' = do
  c <- getContents
  case runParser xmlDocument (lineOffsets' c) "(stdin)" c of
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
