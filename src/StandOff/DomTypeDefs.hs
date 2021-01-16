module StandOff.DomTypeDefs where

import StandOff.LineOffsets
import qualified StandOff.TextRange as TR
import StandOff.MarkupTree

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
         | XMLDeclaration { declaration :: [Attribute]
                          , start :: Position
                          , end :: Position }
         | ProcessingInstruction { name :: String
                                 , declaration :: [Attribute]
                                 , start :: Position
                                 , end :: Position }
         | TextNode { text :: String
                    , start :: Position
                    , end :: Position }
         | Comment { text :: String
                   , start :: Position
                   , end :: Position }
         deriving (Show)

myMapTuple :: (a -> b) -> (a, a) -> (b, b)
myMapTuple f (a1, a2) = (f a1, f a2)

instance TR.TextRange XML where
  start x = posOffset $ fst $ xmlSpanning x
  end x = posOffset $ snd $ xmlSpanning x
  -- Split points have to be corrected. The first part of the split
  -- should always end right before the open tag and the second part
  -- of the split should always start right after a tag, but not at
  -- the position of the tags last char. FIXME: Is this correct for
  -- all markup types?
  splitPoints x = ((so-1, eo+1), (sc-1, ec+1))
    where (so, eo) = myMapTuple posOffset $ elementOpenTagPosition x
          (sc, ec) = myMapTuple posOffset $ elementCloseTagPosition x
  split _ _ = error "Cannot split internal markup"

instance MarkupTree XML where
  getMarkupChildren (Element _ _ _ _ _ _ c) = filter isElementP c
  getMarkupChildren _ = []

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
xmlSpanning (XMLDeclaration _ s e) = (s, e)
xmlSpanning (ProcessingInstruction _ _ s e) = (s, e)

elementOpenTagPosition :: XML -> (Position, Position)
elementOpenTagPosition (Element _ _ s e _ _ _) = (s, e)
elementOpenTagPosition (EmptyElement _ _ s e) = (s, e)

elementCloseTagPosition :: XML -> (Position, Position)
elementCloseTagPosition (Element _ _ _ _ s e _) = (s, e)
elementCloseTagPosition (EmptyElement _ _ s e) = (s, e)

elementContent :: XML -> [XML]
elementContent (Element _ _ _ _ _ _ c) = c
elementContent _ = []

elementWithoutContent :: XML -> XML
elementWithoutContent (Element n a so eo sc ec _) = Element n a so eo sc ec []
elementWithoutContent x = x

textContent :: XML -> String
textContent (TextNode t _ _) = t

-- Is the node an XML element? False for white space, text nodes,
-- processing instructions, xml declarations, comments etc.
isElementP :: XML -> Bool
isElementP (Element _ _ _ _ _ _ _) = True
isElementP (EmptyElement _ _ _ _) = True
isElementP _ = False

isXMLDeclarationP :: XML -> Bool
isXMLDeclarationP (XMLDeclaration _ _ _) = True
isXMLDeclarationP _ = False
