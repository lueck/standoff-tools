{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module StandOff.DomTypeDefs where

import qualified Data.Tree.NTree.TypeDefs as NT
import Data.Tree.Class hiding (getNode)
import qualified Data.Csv as Csv
import Data.Csv ((.=))
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid ((<>))

import StandOff.LineOffsets
import qualified StandOff.TextRange as TR
import StandOff.EquidistantText
import StandOff.ShrinkedText
import StandOff.StringLike (StringLike)
import qualified StandOff.StringLike as SL
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


-- * Equidistant text

instance EquidistantNode XmlNode where
  -- reproduce text nodes
  serializeOpen _ n@(TextNode _ _ _) s =
    (SL.take l s, SL.drop l s)
    where
      l :: Int
      l = openTagLength n
  -- for all other types of nodes we use the filling character
  serializeOpen fillChar n s =
    (SL.pack $ take l $ repeat fillChar, SL.drop l s)
    where
      l :: Int
      l = openTagLength n
  -- close tag is relevant only for 'Element'
  serializeClose fillChar n@(Element _ _ _ _ _ _) s =
    (SL.pack $ take l $ repeat fillChar, SL.drop l s)
    where
      l :: Int
      l = closeTagLength n
  serializeClose _ _ s = (SL.empty, s)


-- * Shrinked text

instance ShrinkingNode XmlNode where
  -- reproduce text nodes
  shrinkOpen _ n@(TextNode _ _ _) s offsets =
    (SL.take l s, (SL.drop l s, offsets <> mapOpenOffsets n l))
    where
      l = openTagLength n
  -- TODO: char refs and entity refs
  -- use replacements for all other kinds of nodes
  shrinkOpen cfg n s offsets =
    (txt, (SL.drop seen s, offsets <> mapOpenOffsets n l))
    where
      txt = shrinkingOpenNodeReplacement cfg n
      l = SL.length txt
      seen = openTagLength n -- length of input seen
  shrinkClose cfg n s offsets = (txt, (SL.drop seen s, offsets <> mapCloseOffsets n l))
    where
      txt = shrinkingCloseNodeReplacement cfg n
      l = SL.length txt
      seen = closeTagLength n -- length of input seen


-- | How to map the offsets of an open tag to a count of new offsets.
-- Should it be [666, 667, 668] or [666, 666, 666] ?
mapOpenOffsets :: XmlNode -> Int -> [Int]
mapOpenOffsets n@(TextNode _ _ _) len = map (+ (TR.start n)) $ take len [0..]
mapOpenOffsets node len = take len $ repeat $ TR.start node
-- mapOffsets strt len = map (+ (TR.start node)) $ take len [0..]

-- | How to map the offsets of a close tag to a count of new offsets.
-- Should it be [666, 667, 668] or [666, 666, 666] ?
mapCloseOffsets :: XmlNode -> Int -> [Int]
mapCloseOffsets node len = take len $ repeat $ TR.end node
-- mapOffsets strt len = map (+ (TR.close node)) $ take len [0..]


-- | Get the replacement for a node's open tag from the config.
shrinkingOpenNodeReplacement
  :: (StringLike s) =>
     ShrinkingNodeConfig s
  -> XmlNode
  -> s
shrinkingOpenNodeReplacement cfg (Element name' _ _ _ _ _) =
  _shrinkRepl_open $ fromMaybe (_shrinkCfg_defaultTagReplacement cfg) $ Map.lookup name' $ _shrinkCfg_tagReplacements cfg
shrinkingOpenNodeReplacement cfg (EmptyElement name' _ _ _) =
  _shrinkRepl_empty $ fromMaybe (_shrinkCfg_defaultTagReplacement cfg) $ Map.lookup name' $ _shrinkCfg_tagReplacements cfg
shrinkingOpenNodeReplacement _ (TextNode txt _ _) =
  SL.pack txt
shrinkingOpenNodeReplacement cfg (XMLDeclaration _ _ _) =
  _shrinkCfg_defaultPiReplacement cfg
shrinkingOpenNodeReplacement cfg (ProcessingInstruction _ _ _ _) =
  _shrinkCfg_defaultPiReplacement cfg
shrinkingOpenNodeReplacement _ (Comment _ _ _) =
  SL.empty


-- | Get the replacement for a node's close tag from the config.
shrinkingCloseNodeReplacement
  :: (StringLike s) =>
     ShrinkingNodeConfig s
  -> XmlNode
  -> s
shrinkingCloseNodeReplacement cfg (Element name' _ _ _ _ _) =
  _shrinkRepl_close $ fromMaybe (_shrinkCfg_defaultTagReplacement cfg) $ Map.lookup name' $ _shrinkCfg_tagReplacements cfg
shrinkingCloseNodeReplacement _ _ = SL.empty


-- * Helper functions

-- | Get the length of the open tag. This returns the length of the
-- open tag of an element node and the length of all other nodes.
openTagLength :: XmlNode -> Int
openTagLength (Element _ _ so eo _ _) = (posOffset eo) - (posOffset so) + 1
openTagLength (EmptyElement _ _ s e) = (posOffset e) - (posOffset s) + 1
openTagLength (XMLDeclaration _ s e) = (posOffset e) - (posOffset s) + 2 -- TODO/FIXME
openTagLength (ProcessingInstruction _ _ s e) = (posOffset e) - (posOffset s) + 2 -- TODO/FIXME
openTagLength (TextNode _ s e) = (posOffset e) - (posOffset s)
openTagLength (Comment _ s e) = (posOffset e) - (posOffset s) + 1

-- | Get the length of the close tag. This returns the length of the
-- close tag of an element node and 0 for all other nodes.
closeTagLength :: XmlNode -> Int
closeTagLength (Element _ _ _ _ sc ec) = (posOffset ec) - (posOffset sc) + 1
closeTagLength _ = 0
