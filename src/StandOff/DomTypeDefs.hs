{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module StandOff.DomTypeDefs where

import qualified Data.Tree.NTree.TypeDefs as NT
import qualified Data.Csv as Csv
import Data.Csv ((.=))
import qualified Data.Vector as V

import StandOff.LineOffsets
import qualified StandOff.TextRange as TR
import StandOff.MarkupTree

type AttrName = String
type AttrVal  = String

data Attribute = Attribute (AttrName, AttrVal) deriving (Show)

data XmlNode
  = Element
    { name :: String
    , attributes ::  [Attribute]
    , startOpenTag :: Position
    , endOpenTag :: Position
    , startCloseTag :: Position
    , endCloseTag :: Position
    }
  | EmptyElement
    { name :: String
    , attributes :: [Attribute]
    , startTag :: Position
    , endTag :: Position
    }
  | XMLDeclaration
    { declaration :: [Attribute]
    , start :: Position
    , end :: Position
    }
  | ProcessingInstruction
    { name :: String
    , declaration :: [Attribute]
    , start :: Position
    , end :: Position
    }
  | TextNode
    { text :: String
    , start :: Position
    , end :: Position }
  | Comment
    { text :: String
    , start :: Position
    , end :: Position
    }
  deriving (Show)

-- | An n-ary tree of 'XML' nodes
type XMLTree = NT.NTree XmlNode

-- | Forest of 'XMLTree'
type XMLTrees = NT.NTrees XmlNode

-- | Get the (current) root node of a 'NTree' subtree.
getNode :: NT.NTree a -> a
getNode (NT.NTree n _) = n


instance TR.TextRange XmlNode where
  start x = posOffset $ fst $ xmlSpanning x
  end x = posOffset $ snd $ xmlSpanning x
  -- Split points have to be corrected. The first part of the split
  -- should always end right before the open tag and the second part
  -- of the split should always start right after a tag, but not at
  -- the position of the tags last char. FIXME: Is this correct for
  -- all markup types?
  splitPoints x = ((so-1, eo+1), (sc-1, ec+1))
    where
      (so, eo) = myMapTuple posOffset $ elementOpenTagPosition x
      (sc, ec) = myMapTuple posOffset $ elementCloseTagPosition x

      myMapTuple :: (a -> b) -> (a, a) -> (b, b)
      myMapTuple f (a1, a2) = (f a1, f a2)

      elementOpenTagPosition :: XmlNode -> (Position, Position)
      elementOpenTagPosition (Element _ _ s e _ _) = (s, e)
      elementOpenTagPosition (EmptyElement _ _ s e) = (s, e)

      elementCloseTagPosition :: XmlNode -> (Position, Position)
      elementCloseTagPosition (Element _ _ _ _ s e) = (s, e)
      elementCloseTagPosition (EmptyElement _ _ s e) = (s, e)
  split _ _ = error "Cannot split internal markup"

instance MarkupTree XMLTree where
  getMarkupChildren (NT.NTree (Element _ _ _ _ _ _) cs) = cs
  getMarkupChildren (NT.NTree _ _) = []

instance Csv.ToField XmlNode where
  toField (Element _ _ _ _ _ _) = Csv.toField (0::Int)
  toField (EmptyElement _ _ _ _) = Csv.toField (1::Int)
  toField (XMLDeclaration _ _ _) = Csv.toField (2::Int)
  toField (ProcessingInstruction _ _ _ _) = Csv.toField (3::Int)
  toField (TextNode _ _ _) = Csv.toField (4::Int)
  toField (Comment _ _ _) = Csv.toField (5::Int)

instance Csv.ToNamedRecord XmlNode where
  toNamedRecord x@(Element n _ so eo sc ec) = Csv.namedRecord
    [ "type" .= x
    ,  "start" .= so
    , "end" .= eo
    , "startClose" .= sc
    , "endClose" .= ec
    , "lname" .= n
    ]
  toNamedRecord x@(EmptyElement n _ s e) = Csv.namedRecord
    [ "type" .= x
    , "start" .= s
    , "end" .= e
    , "startClose" .= (Nothing::Maybe Int)
    , "endClose" .= (Nothing::Maybe Int)
    , "lname" .= n
    ]
  toNamedRecord x@(XMLDeclaration _ s e) = Csv.namedRecord
    [ "type" .= x
    , "start" .= s
    , "end" .= e
    , "startClose" .= (Nothing::Maybe Int)
    , "endClose" .= (Nothing::Maybe Int)
    , "lname" .= (Nothing::Maybe String)
    ]
  toNamedRecord x@(ProcessingInstruction n _ s e) = Csv.namedRecord
    [ "type" .= x
    , "start" .= s
    , "end" .= e
    , "startClose" .= (Nothing::Maybe Int)
    , "endClose" .= (Nothing::Maybe Int)
    , "lname" .= n
    ]
  toNamedRecord x@(TextNode _ s e) = Csv.namedRecord
    [ "type" .= x
    , "start" .= s
    , "end" .= e
    , "startClose" .= (Nothing::Maybe Int)
    , "endClose" .= (Nothing::Maybe Int)
    , "lname" .= (Nothing::Maybe String)
    ]
  toNamedRecord x@(Comment _ s e) = Csv.namedRecord
    [ "type" .= x
    , "start" .= s
    , "end" .= e
    , "startClose" .= (Nothing::Maybe Int)
    , "endClose" .= (Nothing::Maybe Int)
    , "lname" .= (Nothing::Maybe String)
    ]

-- | The order of data presented in CSV
positionHeader :: Csv.Header -- Vector String
positionHeader = V.fromList ["type", "start", "end", "startClose", "endClose", "lname"]


xmlSpanning :: XmlNode -> (Position, Position)
xmlSpanning (Element _ _ s _ _ e) = (s, e)
xmlSpanning (EmptyElement _ _ s e) = (s, e)
xmlSpanning (TextNode _ s e) = (s, e)
xmlSpanning (Comment _ s e) = (s, e)
xmlSpanning (XMLDeclaration _ s e) = (s, e)
xmlSpanning (ProcessingInstruction _ _ s e) = (s, e)


-- Is the node an XML element? False for white space, text nodes,
-- processing instructions, xml declarations, comments etc.
isElementP :: XmlNode -> Bool
isElementP (Element _ _ _ _ _ _) = True
isElementP (EmptyElement _ _ _ _) = True
isElementP _ = False

isXMLDeclarationP :: XmlNode -> Bool
isXMLDeclarationP (XMLDeclaration _ _ _) = True
isXMLDeclarationP _ = False
