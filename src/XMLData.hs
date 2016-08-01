module XMLData where

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

textContent :: XML -> String
textContent (TextNode t _ _) = t

-- Returns a list made from the tree.
flatten :: [XML] -> [XML]
flatten [] = []
flatten ((Element n a so se sc ec inner):xs) =
  (Element n a so se sc ec []) : flatten inner ++ flatten xs
flatten (x:xs) = x : flatten xs
