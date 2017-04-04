{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
module StandOff.Owl
  where

import Text.XML.HXT.Core
import GHC.Generics
import Data.List
import Data.Maybe
import qualified Data.Csv as Csv
import qualified Data.Aeson as A


-- * Type definitions

data Ontology
  = Ontology
    { ontologyId :: Int      -- ^ the ID
    , iri :: String          -- ^ the the ontology IRI
    , versionInfo :: String  -- ^ the version info string
    , xml :: String          -- ^ the serialized ontology XML string
    }
  | OntologyResource
    { ontologyResourceId :: Int -- ^ the ID
    , ontology :: Int        -- ^ the ontology's ID
    , localName :: String    -- ^ the local name
    , resourceType :: String -- ^ the type
    }
  deriving (Show, Eq, Generic)

-- | Test if given parameter is an 'Ontology'.
isOntology :: Ontology -> Bool
isOntology (Ontology _ _ _ _) = True
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
  toRecord (ReadOwl (Ontology _ iri vInfo def))
    = Csv.record [ Csv.toField iri
                 , Csv.toField vInfo
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

-- | Make an 'Ontology' from the root element.
poRoot :: IOSArrow XmlTree Ontology
poRoot =
  isRoot >>>
  getOntologyIri &&&
  (getOntologyVersionInfo `orElse` arr (const "NOVERSION")) &&&
  getOntologyContents >>>
  arr3 (Ontology 0)

-- | owl:Class is parsed into a 'OntologyResource' with 'resourceType'
-- = \"markup\".
poOwlClass :: String -> IOSArrow XmlTree Ontology
poOwlClass ns =
  isElem >>>
  hasQName (mkNsName "Class" owlNs) >>>
  isInOntology ns >>>
  (((getQAttrValue0 (mkNsName "about" rdfNs) >>> arr (stripIri ns)) &&&
    arr (const "markup")) >>>
   arr2 (OntologyResource 0 0))

-- | owl:ObjectProperty is parsed into a 'OntologyResource' with
-- 'resourceType' = \"relation\".
poOwlObjectProperty :: String -> IOSArrow XmlTree Ontology
poOwlObjectProperty ns =
  isElem >>>
  hasQName (mkNsName "ObjectProperty" owlNs) >>>
  isInOntology ns >>>
  ((getQAttrValue0 (mkNsName "about" rdfNs) >>> arr (stripIri ns)) &&&
   arr (const "relation")) >>>
  arr2 (OntologyResource 0 0)

-- | owl:DatatypeProperty is parsed into a 'OntologyResource' with
-- 'resourceType' = \"attribute\".
poOwlDatatypeProperty :: String -> IOSArrow XmlTree Ontology
poOwlDatatypeProperty ns =
  isElem >>>
  hasQName (mkNsName "DatatypeProperty" owlNs) >>>
  isInOntology ns >>>
  ((getQAttrValue0 (mkNsName "about" rdfNs) >>> arr (stripIri ns)) &&&
   arr (const "attribute")) >>>
  arr2 (OntologyResource 0 0)

-- | Parse root or class or object property or ...
poOntology :: IOSArrow XmlTree Ontology
poOntology = poOntology' $< getOntologyIri

poOntology' :: String -> IOSArrow XmlTree Ontology
poOntology' ns =
  poRoot <+>
  multi (poOntology'' ns)

poOntology'' :: String -> IOSArrow XmlTree Ontology
poOntology'' ns =
  poOwlClass ns <+>
  poOwlObjectProperty ns <+>
  poOwlDatatypeProperty ns

-- FIXME: Really always drop 1?
stripIri :: String -> String -> String
stripIri ns qName =
  fromMaybe qName $ fmap (drop 1) $ stripPrefix ns qName

-- | Run the owl parser in the IO monad.
runOwlParser :: FilePath -> IO [Ontology]
runOwlParser fName = do
  results <- runX (readDocument [withValidate no] fName >>>
                   propagateNamespaces >>>
                   poOntology)
  return results
