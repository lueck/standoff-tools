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
    , namespace :: String    -- ^ the namespace URI of the ontology
    , version :: String      -- ^ the version string
    , definition :: String   -- ^ the serialized ontology XML string
    }
  | OntologyResource
    { ontologyResourceId :: Int -- ^ the ID
    , ontology :: Int        -- ^ the ontology's ID
    , localName :: String   -- ^ the local name
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
  toRecord (ReadOwl (Ontology _ ns vsn def))
    = Csv.record [ Csv.toField ns
                 , Csv.toField vsn
                 , Csv.toField def ]
  toRecord (ReadOwl (OntologyResource _ _ ln rt))
    = Csv.record [ Csv.toField ln
                 , Csv.toField rt ]

-- | A wrapper.
data ReadOwl a where
  ReadOwl :: a -> ReadOwl a


-- * Parsing

poRoot :: IOSArrow XmlTree Ontology
poRoot =
  isRoot >>>
  (getOntologyNsUri &&&
   (arr (const "FIXME: Version")) &&&
   (arr (const "FIXME: definition"))) >>>
  arr3 (Ontology 0)

getOntologyNsUri :: (ArrowXml a) => a XmlTree String
getOntologyNsUri =
  this //>
  hasQName (mkNsName "Ontology" owlNs) >>>
  getQAttrValue0 (mkNsName "about" rdfNs)

-- | owl:Class is parsed into a 'OntologyResource' with 'resourceType'
-- = \"markup\".
poOwlClass :: String -> IOSArrow XmlTree Ontology
poOwlClass ns =
  isElem >>>
  hasQName (mkNsName "Class" owlNs) >>>
  ((getQAttrValue0 (mkNsName "about" rdfNs) >>> arr (stripNsUri ns)) &&&
   arr (const "markup")) >>>
  arr2 (OntologyResource 0 0)
  -- FIXME: `when` from ns

-- | owl:ObjectProperty is parsed into a 'OntologyResource' with
-- 'resourceType' = \"relation\".
poOwlObjectProperty :: String -> IOSArrow XmlTree Ontology
poOwlObjectProperty ns =
  isElem >>>
  hasQName (mkNsName "ObjectProperty" owlNs) >>>
  ((getQAttrValue0 (mkNsName "about" rdfNs) >>> arr (stripNsUri ns)) &&&
   arr (const "relation")) >>>
  arr2 (OntologyResource 0 0)

-- | owl:DatatypeProperty is parsed into a 'OntologyResource' with
-- 'resourceType' = \"attribute\".
poOwlDatatypeProperty :: String -> IOSArrow XmlTree Ontology
poOwlDatatypeProperty ns =
  isElem >>>
  hasQName (mkNsName "DatatypeProperty" owlNs) >>>
  ((getQAttrValue0 (mkNsName "about" rdfNs) >>> arr (stripNsUri ns)) &&&
   arr (const "attribute")) >>>
  arr2 (OntologyResource 0 0)

-- | Parse root or class or object property or ...
poOntology :: IOSArrow XmlTree Ontology
poOntology = poOntology' $< getOntologyNsUri

poOntology' :: String -> IOSArrow XmlTree Ontology
poOntology' ns =
  poRoot <+>
  (this //> multi (poOntology'' ns))

poOntology'' :: String -> IOSArrow XmlTree Ontology
poOntology'' ns =
  poOwlClass ns <+>
  poOwlObjectProperty ns <+>
  poOwlDatatypeProperty ns

-- FIXME: Really always drop 1?
stripNsUri :: String -> String -> String
stripNsUri ns qName =
  fromMaybe qName $ fmap (drop 1) $ stripPrefix ns qName

-- | Run the owl parser in the IO monad.
runOwlParser :: FilePath -> IO [Ontology]
runOwlParser fName = do
  results <- runX (readDocument [withValidate no] fName >>>
                   propagateNamespaces >>>
                   poOntology)
  return results
