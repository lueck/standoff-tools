{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module StandOff.DomTypeDefs
  ( AttrName
  , AttrVal
  , Attribute(..)
  , XmlNode(..)
  , NodeType(..)
  , nodeType
  , XMLTree
  , XMLTrees
  , getNode
  , positionHeader
  , isElementP
  , isXMLDeclarationP
  , NamespaceDecl
  , parseNamespaceDecl
  , mkNsEnv
  , mkQNameWithNsEnv
  , validateQName
  , mkShrinkingNodeConfig
  , nodeRange
  ) where

import qualified Data.Tree.NTree.TypeDefs as NT
import Text.XML.HXT.DOM.QualifiedName (QName, XName)
import qualified Text.XML.HXT.DOM.QualifiedName as QN
import qualified Text.XML.HXT.DOM.TypeDefs as XNT
import Data.Tree.Class hiding (getNode)
import qualified Data.Csv as Csv
import Data.Csv ((.=))
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.YAML as Y
import Data.YAML ((.!=), (.:?))
import Data.Char (chr)

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

data XmlNode n s
  -- | an element node with text or element children
  = Element
    { name :: n
    , attributes ::  [Attribute]
    , startOpenTag :: Position
    , endOpenTag :: Position
    , startCloseTag :: Position
    , endCloseTag :: Position
    }
  -- | an empty element (leaf). We introduce this only because it is
  -- distinct from Element in regard to 'TextRange'. This will speed
  -- up processing.
  | EmptyElement
    { name :: n
    , attributes :: [Attribute]
    , start :: Position
    , end :: Position
    }
  -- | an XML declaration (leaf)
  | XMLDeclaration
    { declaration :: [Attribute]
    , start :: Position
    , end :: Position
    }
  -- | a processing instruction (leaf)
  | ProcessingInstruction
    { name :: n
    , declaration :: [Attribute]
    , start :: Position
    , end :: Position
    }
  -- | a text node (leaf)
  | TextNode
    { text :: s
    , start :: Position
    , end :: Position }
  -- | a comment (leaf)
  | Comment
    { text :: s
    , start :: Position
    , end :: Position
    }
  -- | a character reference (leaf)
  | CharRef
    { char :: Int
    , start :: Position
    , end :: Position
    }
  -- | an entity reference (leaf)
  | EntityRef
    { entity :: String -- we use string in order to work with XNT.DTDElem
    , start :: Position
    , end :: Position
    }
  -- | an CData section (leaf)
  | CData
    { text :: s
    , start :: Position
    , end :: Position
    }
  -- | a DTD element with an associative list of elements
  | DTD
    { element :: XNT.DTDElem
    , attrs :: XNT.Attributes
    , start :: Position
    , end :: Position
    }
  deriving (Show)

data NodeType
  = ElementNode
  | EmptyElementNode
  | XMLDeclarationNode
  | ProcessingInstructionNode
  | TextNodeType
  | CommentNode
  | CharRefNode
  | EntityRefNode
  | CDataNode
  | DTDNode
  deriving (Eq)

instance Show NodeType where
  show (ElementNode) = "Element"
  show (EmptyElementNode) = "EmptyElement"
  show (XMLDeclarationNode) = "Decl"
  show (ProcessingInstructionNode) = "PI"
  show (TextNodeType) = "Text"
  show (CommentNode) = "Comment"
  show (CharRefNode) = "CharRef"
  show (EntityRefNode) = "EntityRef"
  show (CDataNode) = "CData"
  show (DTDNode) = "DTD"


-- | A mapping of 'XmlNode' constructors to 'NodeType'.
nodeType :: XmlNode n s -> NodeType
nodeType (Element _ _ _ _ _ _) = ElementNode
nodeType (EmptyElement _ _ _ _) = EmptyElementNode
nodeType (XMLDeclaration _ _ _) = XMLDeclarationNode
nodeType (ProcessingInstruction _ _ _ _) = ProcessingInstructionNode
nodeType (TextNode _ _ _) = TextNodeType
nodeType (Comment _ _ _) = CommentNode
nodeType (CharRef _ _ _) = CharRefNode
nodeType (EntityRef _ _ _) = EntityRefNode
nodeType (CData _ _ _) = CDataNode
nodeType (DTD _ _ _ _) = DTDNode

-- | An n-ary tree of 'XML' nodes
type XMLTree n s = NT.NTree (XmlNode n s)

-- | Forest of 'XMLTree'
type XMLTrees n s = NT.NTrees (XmlNode n s)

-- | Get the (current) root node of a 'NTree' subtree.
getNode :: NT.NTree a -> a
getNode (NT.NTree n _) = n


instance TR.TextRange (XmlNode n s) where
  start x = posOffset $ fst $ nodeRange x
  end x = posOffset $ snd $ nodeRange x
  -- Split points have to be corrected. The first part of the split
  -- should always end right before the open tag and the second part
  -- of the split should always start right after a tag, but not at
  -- the position of the tags last char. FIXME: Is this correct for
  -- all markup types?
  splitPoints x = ((so-1, eo+1), (sc-1, ec+1))
    where
      (so, eo) = myMapTuple posOffset $ openTagRange x
      (sc, ec) = myMapTuple posOffset $ closeTagRange x

      myMapTuple :: (a -> b) -> (a, a) -> (b, b)
      myMapTuple f (a1, a2) = (f a1, f a2)

  split _ _ = error "Cannot split internal markup"

instance MarkupTree (XMLTree n s) where
  getMarkupChildren (NT.NTree (Element _ _ _ _ _ _) cs) = cs
  getMarkupChildren (NT.NTree _ _) = []

instance (Show n) => Csv.ToNamedRecord (XmlNode n s) where
  toNamedRecord n = Csv.namedRecord
    [ "type" .= (show $ nodeType n)
    , "start" .= (fst $ openTagRange n)
    , "end" .= (snd $ openTagRange n)
    , "startClose" .= (fst $ closeTagRange n)
    , "endClose" .= (snd $ closeTagRange n)
    , "lname" .= tagName n
    ]
    where
      tagName :: Show k => XmlNode k s -> Maybe String
      tagName (Element name _ _ _ _ _) = Just $ show name
      tagName (EmptyElement name _ _ _) = Just $ show name
      tagName _ = Nothing


-- | The order of data presented in CSV
positionHeader :: Csv.Header -- Vector String
positionHeader = V.fromList ["type", "start", "end", "startClose", "endClose", "name"]



-- Is the node an XML element? False for white space, text nodes,
-- processing instructions, xml declarations, comments etc.
isElementP :: (XmlNode n s) -> Bool
isElementP (Element _ _ _ _ _ _) = True
isElementP (EmptyElement _ _ _ _) = True
isElementP _ = False

isXMLDeclarationP :: (XmlNode n s) -> Bool
isXMLDeclarationP (XMLDeclaration _ _ _) = True
isXMLDeclarationP _ = False


-- * Equidistant text

instance StringLike s => EquidistantNode (XmlNode n) s where
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

instance (Eq n, Ord n, StringLike s) => ShrinkingNode XmlNode n s where
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
mapOpenOffsets :: (XmlNode n s) -> Int -> [Int]
mapOpenOffsets n@(TextNode _ _ _) len = map (+ (TR.start n)) $ take len [0..]
mapOpenOffsets node len = take len $ repeat $ TR.start node
-- mapOffsets strt len = map (+ (TR.start node)) $ take len [0..]

-- | How to map the offsets of a close tag to a count of new offsets.
-- Should it be [666, 667, 668] or [666, 666, 666] ?
mapCloseOffsets :: (XmlNode n s) -> Int -> [Int]
mapCloseOffsets node len = take len $ repeat $ TR.end node
-- mapOffsets strt len = map (+ (TR.close node)) $ take len [0..]


-- | Get the replacement for a node's open tag from the config.
shrinkingOpenNodeReplacement
  :: (StringLike s, Ord k) =>
     ShrinkingNodeConfig k s
  -> (XmlNode k s)
  -> s
shrinkingOpenNodeReplacement cfg (Element name' _ _ _ _ _) =
  _shrinkRepl_open $ fromMaybe (_shrinkCfg_defaultTagReplacement cfg) $ Map.lookup name' $ _shrinkCfg_tagReplacements cfg
shrinkingOpenNodeReplacement cfg (EmptyElement name' _ _ _) =
  _shrinkRepl_empty $ fromMaybe (_shrinkCfg_defaultTagReplacement cfg) $ Map.lookup name' $ _shrinkCfg_tagReplacements cfg
shrinkingOpenNodeReplacement _ (TextNode txt _ _) = txt
shrinkingOpenNodeReplacement _ (CharRef c _ _) = SL.singleton $ chr c
shrinkingOpenNodeReplacement cfg (EntityRef entity _ _) = SL.empty -- TODO / FIXME
-- all other types of nodes are muted
shrinkingOpenNodeReplacement _ _ = SL.empty


-- | Get the replacement for a node's close tag from the config.
shrinkingCloseNodeReplacement
  :: (StringLike s, Ord k) =>
     ShrinkingNodeConfig k s
  -> (XmlNode k s)
  -> s
shrinkingCloseNodeReplacement cfg (Element name' _ _ _ _ _) =
  _shrinkRepl_close $ fromMaybe (_shrinkCfg_defaultTagReplacement cfg) $ Map.lookup name' $ _shrinkCfg_tagReplacements cfg
shrinkingCloseNodeReplacement _ _ = SL.empty


-- * Qualified names

-- | Albeit it may be slower than using Data.Text and Data.Map, we use
-- 'QName' and 'QN.NsEnv' from the hxt package to represent qualified
-- names. In configuration files we often need namespace
-- declarations. To parse them from yaml we have @parseNamespaceDecl@.
--
-- Usage notes:
--
-- @validateQName . mkQNameWithNsEnv (mkNsEnv nsDecl)@ where
-- @nsDecl::NamespaceDecl@ can be composed this way to make qualified
-- names in a 'ShrinkingNodeConfig' using 'adaptShrinkingConfig'.


-- | A type for namespace declarations. It has a default namespace and
-- a mapping of prefixes to namespaces.
data NamespaceDecl s = NamespaceDecl (Maybe s) (Map.Map s s)
  deriving (Eq, Show)

instance Y.FromYAML (NamespaceDecl Text) where
  parseYAML = Y.withMap "namespaces" $ \m -> NamespaceDecl
    <$> m .:? "default-namespace" .!= Just "Hallo"
    <*> m .:? "prefixes" .!= Map.empty

-- | Parse a namespace declaration from yaml.
parseNamespaceDecl :: Monad m => BL.ByteString -> m (NamespaceDecl Text)
parseNamespaceDecl c = do
  case Y.decode c of
    Left (pos, err) -> fail $ show err ++ " " ++ show pos
    Right cfg -> do
      return $ head cfg

-- | Make a 'QN.NsEnv' out of a (parsed) namespace declaration.
mkNsEnv :: StringLike s => NamespaceDecl s -> QN.NsEnv
mkNsEnv (NamespaceDecl def decl) =
  QN.toNsEnv $ map ((,) <$> SL.unpack . fst <*> SL.unpack . snd) $ appendDefault def $ Map.toList decl
  where
    appendDefault Nothing = id
    appendDefault (Just d) = ((SL.empty,d):)

-- | Albeit it's slow, we want to use 'QN.NsEnv' and most of hxt's
-- module for qualified names for propagating namespaces.
mkNsEnv' :: Map.Map Text Text -> QN.NsEnv
mkNsEnv' nss = map ((,) <$> QN.newXName . T.unpack . fst <*> QN.newXName . T.unpack . snd) $ Map.toList nss

-- | Make a 'QName' from a string and a mapping of prefixes to
-- namespace names.
mkQNameWithNsEnv :: StringLike s => QN.NsEnv -> s -> QName
mkQNameWithNsEnv nsEnv = QN.setNamespace nsEnv . QN.mkName . SL.unpack

-- | Validate a 'QName' and report errors if it's invalid.
validateQName :: QName -> Either String QName
validateQName qn
  | not $ QN.isDeclaredNamespace qn = Left $ "Undefined prefix: " ++ show qn
  | not $ QN.isWellformedNameSpaceName qn = Left $ "No wellformed namespace name: " ++ show qn
  | not $ QN.isWellformedQName qn = Left $ "No wellformed NCname: " ++ show qn
  | otherwise = Right qn


mkShrinkingNodeConfig
  :: (Ord n, Monad m) =>
     (NamespaceDecl Text -> Text -> Either String n)
  -> (Text -> Either String s)
  -> BL.ByteString
  -> m (ShrinkingNodeConfig n s)
mkShrinkingNodeConfig nameFun textFun c = do
  nsDecl <- parseNamespaceDecl c
  shrinkCfg <- parseShrinkingConfig c
  case adaptShrinkingConfig (nameFun nsDecl) textFun shrinkCfg of
    Left err -> fail err
    Right cfg -> return cfg


-- * Helper functions

-- | The starting and ending position of the overall node.
nodeRange :: (XmlNode n s) -> (Position, Position)
nodeRange (Element _ _ s _ _ e) = (s, e)
nodeRange n = openTagRange n

-- | The starting and ending position of the open tag in case of
-- non-empty element nodes and the overall nodes in case of a leaf
-- node.
openTagRange :: (XmlNode n s) -> (Position, Position)
openTagRange (Element _ _ s e _ _) = (s, e)
openTagRange (EmptyElement _ _ s e) = (s, e)
openTagRange (XMLDeclaration _ s e) = (s, e)
openTagRange (ProcessingInstruction _ _ s e) = (s, e)
openTagRange (TextNode _ s e) = (s, e)
openTagRange (Comment _ s e) = (s, e)
openTagRange (CharRef _ s e) = (s, e)
openTagRange (EntityRef _ s e) = (s, e)
openTagRange (CData _ s e) = (s, e)
openTagRange (DTD _ _ s e) = (s, e)

-- | The starting and ending position of the close tag in case of
-- non-empty element nodes *and the same as 'opentag' otherwise.
closeTagRange :: (XmlNode n s) -> (Position, Position)
closeTagRange (Element _ _ _ _ s e) = (s, e)
closeTagRange n = openTagRange n


-- | Get the length of the open tag. This returns the length of the
-- open tag of an element node and the length of all other nodes.
openTagLength :: XmlNode n s -> Int
openTagLength n = posOffset e - posOffset s + 1
  where
    (s, e) = openTagRange n

-- | Get the length of the close tag. This returns the length of the
-- close tag of an element node and 0 for all other nodes.
closeTagLength :: (XmlNode n s) -> Int
closeTagLength (Element _ _ _ _ sc ec) = (posOffset ec) - (posOffset sc) + 1
closeTagLength _ = 0
