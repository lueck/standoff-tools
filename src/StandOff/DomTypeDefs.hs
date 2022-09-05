{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module StandOff.DomTypeDefs
  ( Attribute(..)
  , XmlNode(..)
  , NodeType(..)
  , nodeType
  , XMLTree
  , XMLTrees
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
  , openTagRange
  , closeTagRange
  ) where

import qualified Data.Tree.NTree.TypeDefs as NT
import Text.XML.HXT.DOM.QualifiedName (QName, XName)
import qualified Text.XML.HXT.DOM.QualifiedName as QN
import qualified Text.XML.HXT.DOM.TypeDefs as XNT
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

import qualified StandOff.TextRange as TR
import StandOff.EquidistantText
import StandOff.ShrinkedText
import StandOff.StringLike (StringLike)
import qualified StandOff.StringLike as SL
import StandOff.MarkupTree

-- | An attribute is a name value pair
data Attribute n v = Attribute n v deriving (Show)

-- | For representind XML nodes 'XmlNode' is parametrized with a
-- position @p@, a node name type @n@ and a string-like type for text
-- nodes @s@.
data XmlNode p n s
  -- | an element node with text or element children
  = Element
    { name :: n
    , attributes ::  [Attribute n s]
    , startOpenTag :: p
    , endOpenTag :: p
    , startCloseTag :: p
    , endCloseTag :: p
    }
  -- | an empty element (leaf). We introduce this only because it is
  -- distinct from Element in regard to 'TextRange'. This will speed
  -- up processing.
  | EmptyElement
    { name :: n
    , attributes :: [Attribute n s]
    , start :: p
    , end :: p
    }
  -- | an XML declaration (leaf)
  | XMLDeclaration
    { declaration :: [Attribute n s]
    , start :: p
    , end :: p
    }
  -- | a processing instruction (leaf)
  | ProcessingInstruction
    { target :: n
    , instruction :: s
    , start :: p
    , end :: p
    }
  -- | a text node (leaf)
  | TextNode
    { text :: s
    , start :: p
    , end :: p }
  -- | a comment (leaf)
  | Comment
    { text :: s
    , start :: p
    , end :: p
    }
  -- | a character reference (leaf)
  | CharRef
    { char :: Int
    , start :: p
    , end :: p
    }
  -- | an entity reference (leaf)
  | EntityRef
    { entity :: String -- we use string in order to work with XNT.DTDElem
    , start :: p
    , end :: p
    }
  -- | an CData section (leaf)
  | CData
    { text :: s
    , start :: p
    , end :: p
    }
  -- | a DTD element with an associative list of elements
  | DTD
    { element :: XNT.DTDElem
    , attrs :: XNT.Attributes
    , start :: p
    , end :: p
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
nodeType :: XmlNode p n s -> NodeType
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
type XMLTree p n s = NT.NTree (XmlNode p n s)

-- | Forest of 'XMLTree'
type XMLTrees p n s = NT.NTrees (XmlNode p n s)


instance TR.TextRange ((XmlNode Int) n s) where
  start x = fst $ nodeRange x
  end x = snd $ nodeRange x
  -- Split points have to be corrected. The first part of the split
  -- should always end right before the open tag and the second part
  -- of the split should always start right after a tag, but not at
  -- the position of the tags last char. FIXME: Is this correct for
  -- all markup types?
  splitPoints x = ((so-1, eo+1), (sc-1, ec+1))
    where
      (so, eo) = openTagRange x
      (sc, ec) = closeTagRange x

  -- | A text node does not have any restricted text ranges; an
  -- non-empty element node has two restricted ranges namely the tags;
  -- all other nodes are completely restricted.
  restrictedRanges n@(Element _ _ _ _ _ _) = [(openTagRange n), (closeTagRange n)]
  restrictedRanges (TextNode _ _ _) = []
  restrictedRanges n = [(TR.spans n)]

  -- prohibited (Element _ _ _ _ _ _) = False
  -- prohibited (TextNode _ _ _) = False
  -- probibited _ = True

  split _ _ = error "Cannot split internal markup"

instance MarkupTree NT.NTree (XmlNode Int n s) where
  getChildren (NT.NTree _ cs) = cs
  getNode (NT.NTree n _) = n

instance (Show n, Show p) => Csv.ToNamedRecord (XmlNode p n s) where
  toNamedRecord n = Csv.namedRecord
    [ "type" .= (show $ nodeType n)
    , "start" .= (show $ fst $ openTagRange n)
    , "end" .= (show $ snd $ openTagRange n)
    , "startClose" .= (show $ fst $ closeTagRange n)
    , "endClose" .= (show $ snd $ closeTagRange n)
    , "name" .= tagName n
    ]
    where
      tagName :: Show k => XmlNode p k s -> Maybe String
      tagName (Element name _ _ _ _ _) = Just $ show name
      tagName (EmptyElement name _ _ _) = Just $ show name
      tagName _ = Nothing


-- | The order of data presented in CSV
positionHeader :: Csv.Header -- Vector String
positionHeader = V.fromList ["type", "start", "end", "startClose", "endClose", "name"]



-- Is the node an XML element? False for white space, text nodes,
-- processing instructions, xml declarations, comments etc.
isElementP :: (XmlNode p n s) -> Bool
isElementP (Element _ _ _ _ _ _) = True
isElementP (EmptyElement _ _ _ _) = True
isElementP _ = False

isXMLDeclarationP :: (XmlNode p n s) -> Bool
isXMLDeclarationP (XMLDeclaration _ _ _) = True
isXMLDeclarationP _ = False


-- * Equidistant text

instance StringLike s => EquidistantNode (XmlNode Int n) s where
  -- reproduce text nodes
  serializeOpen _ n@(TextNode _ _ _) s =
    (SL.take l s, SL.drop l s)
    where
      l :: Int
      l = openTagLength id n
  -- for all other types of nodes we use the filling character
  serializeOpen fillChar n s =
    (SL.pack $ take l $ repeat fillChar, SL.drop l s)
    where
      l :: Int
      l = openTagLength id n
  -- close tag is relevant only for 'Element'
  serializeClose fillChar n@(Element _ _ _ _ _ _) s =
    (SL.pack $ take l $ repeat fillChar, SL.drop l s)
    where
      l :: Int
      l = closeTagLength id n
  serializeClose _ _ s = (SL.empty, s)


-- * Shrinked text

instance (Eq n, Ord n, StringLike s) => ShrinkingNode (XmlNode Int) n s where
  shrinkOpen _ n@(TextNode _ _ _) s offsets =
    (SL.take l s, (SL.drop l s, offsets <> mapOpenOffsets n l))
    where
      l = openTagLength id n
  -- TODO: char refs and entity refs
  -- use replacements for all other kinds of nodes
  shrinkOpen cfg n s offsets =
    (txt, (SL.drop seen s, offsets <> mapOpenOffsets n l))
    where
      txt = shrinkingOpenNodeReplacement cfg n
      l = SL.length txt
      seen = openTagLength id n -- length of input seen
  shrinkClose cfg n s offsets = (txt, (SL.drop seen s, offsets <> mapCloseOffsets n l))
    where
      txt = shrinkingCloseNodeReplacement cfg n
      l = SL.length txt
      seen = closeTagLength id n -- length of input seen


-- | How to map the offsets of an open tag to a count of new offsets.
-- Should it be [666, 667, 668] or [666, 666, 666] ?
mapOpenOffsets :: (XmlNode Int n s) -> Int -> [Int]
mapOpenOffsets n@(TextNode _ _ _) len = map (+ (TR.start n)) $ take len [0..]
mapOpenOffsets node len = take len $ repeat $ TR.start node
-- mapOffsets strt len = map (+ (TR.start node)) $ take len [0..]

-- | How to map the offsets of a close tag to a count of new offsets.
-- Should it be [666, 667, 668] or [666, 666, 666] ?
mapCloseOffsets :: (XmlNode Int n s) -> Int -> [Int]
mapCloseOffsets node len = take len $ repeat $ TR.end node
-- mapOffsets strt len = map (+ (TR.close node)) $ take len [0..]


-- | Get the replacement for a node's open tag from the config.
shrinkingOpenNodeReplacement
  :: (StringLike s, Ord k) =>
     ShrinkingNodeConfig k s
  -> (XmlNode p k s)
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
  -> (XmlNode p k s)
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
nodeRange :: (XmlNode p n s) -> (p, p)
nodeRange (Element _ _ s _ _ e) = (s, e)
nodeRange n = openTagRange n

-- | The starting and ending position of the open tag in case of
-- non-empty element nodes and the overall nodes in case of a leaf
-- node.
openTagRange :: (XmlNode p n s) -> (p, p)
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
closeTagRange :: (XmlNode p n s) -> (p, p)
closeTagRange (Element _ _ _ _ s e) = (s, e)
closeTagRange n = openTagRange n


-- | Get the length of the open tag. This returns the length of the
-- open tag of an element node and the length of all other nodes.
openTagLength :: (p -> Int) -> XmlNode p n s -> Int
openTagLength f n = f e - f s + 1
  where
    (s, e) = openTagRange n

-- | Get the length of the close tag. This returns the length of the
-- close tag of an element node and 0 for all other nodes.
closeTagLength :: (p -> Int) -> (XmlNode p n s) -> Int
closeTagLength f (Element _ _ _ _ sc ec) = (f ec) - (f sc) + 1
closeTagLength _ _ = 0
