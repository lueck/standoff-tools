{-# LANGUAGE CPP, FlexibleContexts #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import qualified Text.Parsec as P
import Data.Functor.Identity (Identity)
import Data.Aeson (encode, toJSON)
import qualified Data.ByteString.Lazy as B
import Language.Haskell.TH.Ppr (bytesToString)

import StandOff.XML.NodeOffsets (xmlDocument)
import StandOff.XML.LineOffsets (lineOffsets, Position, posOffset)
import StandOff.ELisp.DumpFile (elDump)
import StandOff.Internalizer.Internalize (internalize)
import StandOff.XML.TagSerializer
import StandOff.XML.AttributeSerializer
import StandOff.Data.Annotation (makeAttributiveRanges, isMarkupRangeP, isRelationP, isPredicateP)
import StandOff.Data.XML (XML, isXMLDeclarationP, isElementP, xmlSpanning)
import StandOff.Data.Tag (NSNameValueSerializer(..))

data OutputFormat = Raw
  | Json
  deriving (Eq, Show)

data AnnotationTypes = AllAnnotations
  | Ranges
  | Relations
  | Predicates
  | ButRanges
  deriving (Eq, Show)

data Serializer = Simple
  | RDF String
  | Namespace String
  | TEI String
  deriving (Eq, Show)

data Options = Options
  { optCommand :: Command
  } deriving (Eq, Show) 

data Command = Offsets String
  | Dumped (Maybe OutputFormat) (Maybe AnnotationTypes) String
  | Internalize Serializer (Maybe String) String String
  deriving (Eq, Show)

rawOutput_ :: Parser OutputFormat
rawOutput_ = flag' Raw (short 'w' <> long "raw-output" <> help "Raw output format")

jsonOutput_ :: Parser OutputFormat
jsonOutput_ = flag' Json (short 'j' <> long "json-output" <> help "JSON output format")

outputFormat_ :: Parser OutputFormat
outputFormat_ = rawOutput_ <|> jsonOutput_

allAnnotations_ :: Parser AnnotationTypes
allAnnotations_ = flag' AllAnnotations (short 'a' <> long "all-annotations" <> help "Parse all annotation types dumped in the elisp dump file.")

rangeAnnotations_ :: Parser AnnotationTypes
rangeAnnotations_ =
  flag' Ranges (short 'r' <>
                long "ranges" <>
                help "Filter the output, so that only markup ranges are returned.")

relationAnnotations_ :: Parser AnnotationTypes
relationAnnotations_ =
  flag' Relations (short 'l' <>
                   long "relations" <>
                   help "Filter the output, so that relations are returned.")

predicateAnnotations_ :: Parser AnnotationTypes
predicateAnnotations_ =
  flag' Predicates (short 'p' <>
                    long "predicates" <>
                    help "Filter the output, so that only literal predicates are returned.")

butRangeAnnotations_ :: Parser AnnotationTypes
butRangeAnnotations_ =
  flag' ButRanges (short 'R' <>
                   long "but-ranges" <>
                   help "Filter the output, so that only non-range annotation types (i.e. relations and predicates) are returned.")

annotationTypes_ :: Parser AnnotationTypes
annotationTypes_ = rangeAnnotations_ <|> relationAnnotations_ <|> predicateAnnotations_ <|> butRangeAnnotations_

offsets_ :: Parser Command
offsets_ = Offsets <$> argument str (metavar "FILE")

dumped_ :: Parser Command
dumped_ = Dumped
  <$> optional outputFormat_
  <*> optional annotationTypes_
  <*> argument str (metavar "FILE")

serializer_ :: Parser Serializer
serializer_ = simpleSerializer_ <|> namespaceSerializer_ <|> rdfSerializer_ <|> teiSerializer_

simpleSerializer_ :: Parser Serializer
simpleSerializer_ = flag' Simple (short 's' <> long "simple" <> help "Simple serializer only usefull for tags without namespaces.")

namespaceSerializer_ :: Parser Serializer
namespaceSerializer_ =
  Namespace <$> strOption ( short 'p'
                            <> long "prefix"
                            <> help "Serialize namespaces for each markup tag using PREFIX.")

rdfSerializer_ :: Parser Serializer
rdfSerializer_ =
  RDF <$> strOption ( short 'r'
                      <> long "rdf"
                      <> help "Serialize markup tags into elements of type ELEMENT writing the markup type as an \'rdf:a\'-attribute.")

teiSerializer_ :: Parser Serializer
teiSerializer_ =
  TEI <$> strOption ( short 't'
                      <> long "tei"
                      <> help "Serialize markup tags into elements of type ELEMENT writing the markup type (local name) as a rendition-attribute.")

internalize_ :: Parser Command
internalize_ = Internalize
  <$> serializer_
  <*> optional (strOption ( short 'i'
                             <> long "processing-instruction"
                             <> help "Insert a processing instruction into the result."))
  <*> argument str (metavar "DUMPFILE")
  <*> argument str (metavar "SOURCE")
  
command_ :: Parser Command
command_ = subparser
  ( command "dumped"
    (info (dumped_ <**> helper)
      (fullDesc <>
       progDesc "Reads annotations generated with GNU Emacs' standoff-mode and dumped in FILE. The output can be formatted in raw (default) or in JSON format. The output can be filtered for annotation types. If no filtering option is given, all annotation types are returned in the output." <>
       header "standoff dumped - a parser for annotations dumped in an emacs lisp file."))
    <> command "internalize"
    (info (internalize_ <**> helper)
      (fullDesc <>
       progDesc "Internalize external annotations given in DUMPFILE into SOURCE. DUMPFILE must be generated (or must look like it's been generated) with GNU Emacs' standoff-mode. SOURCE must be a valid XML file, at least it must contain a root node. There are options on how the internalizer should serialize markup ranges, its type information, IDs etc. By default only markup ranges are internalized, but not relations." <>
       header "standoff internalize - internalize standoff markup into an xml file."))
    <> command "offsets"
    (info (offsets_ <**> helper)
      (fullDesc <>
       progDesc "Returns the character offsets, lines and columns of the nodes of an XML file." <>
       header "standoff offsets - an xml parser returning node positions."))
  )

parseGeneric :: (Monad m, P.Stream s Identity t) => String -> P.Parsec s u a -> u -> P.SourceName -> s -> m a
parseGeneric err parser state fName contents = do
  case P.runParser parser state fName contents of
    Left e -> fail (err ++ " (" ++ fName ++ ") : " ++ (show e))
    Right r -> return r

run :: Command -> IO ()
run (Offsets fileName) = do
  c <- readFile fileName
  offsets <- parseGeneric "Error parsing line lengths" lineOffsets () fileName c
  offsets' <- parseGeneric "Error parsing XML input" xmlDocument offsets fileName c
  print offsets'
run (Dumped oFormat aTypes fileName) = do
  c <- readFile fileName
  annotations <- parseGeneric "Error parsing elisp dump file" elDump () fileName c
  putStr $ formatter $ filter annotationsFilter annotations
  where formatter = case oFormat of
          Just Json -> bytesToString . B.unpack . encode
          otherwise -> show
        annotationsFilter = case aTypes of
          Just Ranges -> isMarkupRangeP
          Just Relations -> isRelationP
          Just Predicates -> isPredicateP
          Just ButRanges -> isRelationP -- FIXME: as soon as we have Predicates
          otherwise -> isAnnotationP
        isAnnotationP _ = True
run (Internalize slizer procInstr dumpFile xmlFile) = do
  dumpContents <- readFile dumpFile
  dumped <- parseGeneric "Error parsing elisp dump file" elDump () dumpFile dumpContents
  xmlContents <- readFile xmlFile
  lOffsets <- parseGeneric "Error parsing line lengths" lineOffsets () xmlFile xmlContents
  xml <- parseGeneric "Error parsing XML input" xmlDocument lOffsets xmlFile xmlContents
  putStr (insertAt
           (internalize
             xmlContents
             (filter isElementP xml)
             (makeAttributiveRanges dumped)
             slizer')
           procInstr
           (behindXMLDeclOrTop xml))
  where slizer' = case slizer of
                    Simple -> serializeTag serializeAttributes'
                    -- FIXME: define tag serializers for attributes and options
                    RDF elName -> serializeSpanTag serializeAttributes' elName
                    Namespace prefix -> serializeNsTag serializeAttributes' prefix
                    TEI elName -> serializeSpanTag teiAttributes elName 
        insertAt s (Just new) pos = (take pos s) ++ "\n" ++ new ++ (drop (pos) s)
        insertAt s Nothing _ = s
        behindXMLDeclOrTop xml
          | length decl == 1 = (posOffset $ snd $ xmlSpanning $ head decl) - 1
          | otherwise = 0
          where decl = filter isXMLDeclarationP xml
        teiAttributes = (serializeAttributes
                          (Just "id")
                          (Just "eid")
                          (Just "rendition")
                          LocalName)

opts :: ParserInfo Command
opts = info
       (command_ <**> helper)
       (fullDesc <>
         header "standoff - a tool for handling standoff annotations (aka external markup)." <>
         progDesc "standoff offers commands for parsing a dump file that contains external markup and for internalizing such external markup into an xml file. There is also a command for parsing a file for newlines, which may be useful for debugging." <>
         footer "See also: https://github.com/lueck/standoff-mode/ - standoff-mode is a tagger for GNU Emacs.")

main :: IO ()
main = execParser opts >>= run
