{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
module StandOff.Owl
  where

import Text.XML.HXT.Core
import GHC.Generics
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Csv as Csv
import qualified Data.Aeson as A
import qualified Data.ByteString as B


-- * Type definitions

data Ontology
  = Ontology
    { ontologyId :: Int      -- ^ the ID
    , iri :: String          -- ^ the the ontology IRI
    , namespace_delimiter :: Maybe String -- ^ delimiting character between namespace and local name, see https://www.w3.org/2001/sw/BestPractices/VM/http-examples/2006-01-18/#naming
    , prefix :: Maybe String -- ^ the prefix of IRI(#|/)
    , versionInfo :: Maybe String  -- ^ the version info string
    , xml :: String          -- ^ the serialized ontology XML string
    }
  | OntologyResource
    { ontologyResourceId :: Int -- ^ the ID
    , ontology :: Int           -- ^ the ontology's ID
    , localName :: String       -- ^ the local name
    , resourceType :: String    -- ^ the type
    }
  deriving (Show, Eq, Generic)

-- | Test if given parameter is an 'Ontology'.
isOntology :: Ontology -> Bool
isOntology (Ontology _ _ _ _ _ _) = True
isOntology _ = False

-- | Test if given parameter is an 'OntologyResource'.
isOntologyResource :: Ontology -> Bool
isOntologyResource (OntologyResource _ _ _ _) = True
isOntologyResource _ = False

-- | Test if given parameter is a markup resource.
isMarkup :: Ontology -> Bool
isMarkup (OntologyResource _ _ _ "markup") = True
isMarkup _ = False

-- | Test if given parameter is a relation resource.
isRelation :: Ontology -> Bool
isRelation (OntologyResource _ _ _ "relation") = True
isRelation _ = False

-- | Test if given parameter is a attribute resource.
isAttribute :: Ontology -> Bool
isAttribute (OntologyResource _ _ _ "attribute") = True
isAttribute _ = False


-- * Namespaces

owlNs :: String
owlNs = "http://www.w3.org/2002/07/owl#"

rdfNs :: String
rdfNs = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"


-- * Exporting

-- | 'Ontology' is ready to be exported to JSON.
instance A.ToJSON Ontology

-- | 'Ontology' is ready to be exported to CSV.
instance Csv.ToRecord Ontology

-- | When wrapped in 'ReadOwl' 'Ontology' is exported to CSV without
-- fields that are set by the database.
instance Csv.ToRecord (ReadOwl Ontology) where
  toRecord (ReadOwl (Ontology _ iri delim pfx vInfo def))
    = Csv.record [ Csv.toField iri
                 , maybe B.empty Csv.toField delim                 
                 , maybe B.empty Csv.toField pfx
                 , maybe B.empty Csv.toField vInfo
                 , Csv.toField def ]
  toRecord (ReadOwl (OntologyResource _ _ ln rt))
    = Csv.record [ Csv.toField ln
                 , Csv.toField rt ]

-- | A wrapper.
data ReadOwl a where
  ReadOwl :: a -> ReadOwl a


-- * Parsing

-- | Get the rdf:about value of the owl:Ontology element, i.e. the
-- ontology IRI.
getOntologyIri :: (ArrowXml a) => a XmlTree String
getOntologyIri =
  deep (hasQName (mkNsName "Ontology" owlNs)) >>>
  getQAttrValue0 (mkNsName "about" rdfNs)

-- | Get the owl:versionInfo from the owl:Ontology element.
getOntologyVersionInfo :: (ArrowXml a) => a XmlTree String
getOntologyVersionInfo =
  deep (hasQName (mkNsName "Ontology" owlNs)) >>>
  getChildren >>>
  hasQName (mkNsName "versionInfo" owlNs) >>>
  getChildren >>>
  getText

-- FIXME: Is readFile sufficient, or do we need handlers for stdin,
-- http, https etc.?
getOntologyContents :: IOSArrow XmlTree String
getOntologyContents = (getAttrValue0 a_source >>> arrIO readFile)

-- | Filters out elements thats rdf:about identifier is not from the
-- ontology iri.
isInOntology :: (ArrowXml a) => String -> a XmlTree XmlTree
isInOntology iri =
  (getQAttrValue (mkNsName "about" rdfNs) >>> isA iriPrefixP)
  `guards` this
  where
    iriPrefixP = isJust . (stripPrefix iri)

-- | Strip the namespace given as first parameter from the qualified
-- name given as second parameter. The qualified name is returned, if
-- it is not prefixed with the namespace.
stripIri :: String -> String -> String
stripIri ns qName =
  fromMaybe qName $ stripPrefix ns qName

-- | Get the rdf:about attribute.
getRdfAbout :: (ArrowXml a) => a XmlTree String
getRdfAbout = getQAttrValue0 (mkNsName "about" rdfNs)

-- | Get the namespace delimiter. Is can be expected to be a hash or
-- slash.
getNamespaceDelimiter :: (ArrowXml a) => String -> a XmlTree (Maybe String)
getNamespaceDelimiter ns =
  single (deep (isElem >>>
                hasNameWith ((/="ontology") . (map toLower) . localPart) >>>
                isInOntology ns)) >>>
  -- FIXME: Should we (takeWhile isPunctuation) instead of (take 1)?
  getRdfAbout >>> arr ((fmap (take 1)) . (stripPrefix ns))

-- | Get the prefix for given namespace followed be @/@ or @#@.
getPrefix :: (ArrowXml a) => String -> a XmlTree (Maybe String)
getPrefix ns =
  isRoot >>>
  getChildren >>>
  isElem >>>
  getAttrl >>>
  isAttr >>>
  -- filter out default namespace
  hasNameWith ((/= "xmlns") . (map toLower) . localPart) >>>
  -- filter out namespace = ns, but not namespace = ns(#|/)
  ((xshow getChildren >>> isA (`elem` [ns++"#", ns++"/"])) `guards` this) >>>
  getQName >>> arr (Just . localPart)

-- | construction of a 5 argument arrow from a 5-ary function. See
-- 'arr4'.
arr5 :: (Arrow a) => (b1 -> b2 -> b3 -> b4 -> b5 -> c) -> a (b1, (b2, (b3, (b4, b5)))) c
arr5 f = arr (\ ~(x1, ~(x2, ~(x3, ~(x4, x5)))) -> f x1 x2 x3 x4 x5)
{-# INLINE arr5 #-}


-- | Make an 'Ontology' from the root element.
poRoot :: String -> IOSArrow XmlTree Ontology
poRoot ns =
  isRoot >>>
  arr (const ns) &&&
  (getNamespaceDelimiter ns `orElse` arr (const Nothing)) &&&
  (getPrefix ns `orElse` arr (const Nothing)) &&&
  ((getOntologyVersionInfo >>> arr Just) `orElse` arr (const Nothing)) &&&
  getOntologyContents >>>
  arr5 (Ontology 0)

-- | owl:Class is parsed into a 'OntologyResource' with 'resourceType'
-- = \"markup\".
poOwlClass :: String -> IOSArrow XmlTree Ontology
poOwlClass ns =
  isElem >>>
  hasQName (mkNsName "Class" owlNs) >>>
  isInOntology ns >>>
  (getRdfAbout >>> arr ((drop 1) . (stripIri ns))) &&&
  arr (const "markup") >>>
  arr2 (OntologyResource 0 0)

-- | owl:ObjectProperty is parsed into a 'OntologyResource' with
-- 'resourceType' = \"relation\".
poOwlObjectProperty :: String -> IOSArrow XmlTree Ontology
poOwlObjectProperty ns =
  isElem >>>
  hasQName (mkNsName "ObjectProperty" owlNs) >>>
  isInOntology ns >>>
  (getRdfAbout >>> arr ((drop 1) . (stripIri ns))) &&&
  arr (const "relation") >>>
  arr2 (OntologyResource 0 0)

-- | owl:DatatypeProperty is parsed into a 'OntologyResource' with
-- 'resourceType' = \"attribute\".
poOwlDatatypeProperty :: String -> IOSArrow XmlTree Ontology
poOwlDatatypeProperty ns =
  isElem >>>
  hasQName (mkNsName "DatatypeProperty" owlNs) >>>
  isInOntology ns >>>
  (getRdfAbout >>> arr ((drop 1) . (stripIri ns))) &&&
  arr (const "attribute") >>>
  arr2 (OntologyResource 0 0)

-- | Parse root or class or object property or ...
poOntology :: IOSArrow XmlTree Ontology
poOntology = poOntology' $< getOntologyIri

poOntology' :: String -> IOSArrow XmlTree Ontology
poOntology' ns =
  poRoot ns <+>
  multi (poOntology'' ns)

poOntology'' :: String -> IOSArrow XmlTree Ontology
poOntology'' ns =
  poOwlClass ns <+>
  poOwlObjectProperty ns <+>
  poOwlDatatypeProperty ns

-- | Run the owl parser in the IO monad.
runOwlParser :: FilePath -> IO [Ontology]
runOwlParser fName = do
  results <- runX (readDocument [withValidate no] fName >>>
                   propagateNamespaces >>>
                   poOntology)
  return results
