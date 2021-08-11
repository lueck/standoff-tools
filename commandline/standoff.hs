{-# LANGUAGE CPP, FlexibleContexts #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Data.Char
import qualified Text.Parsec as P
import Data.Functor.Identity (Identity)
import Data.Aeson (encode, toJSON)
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as B
import Language.Haskell.TH.Ppr (bytesToString)

import StandOff.XmlParsec (runXmlParser)
import StandOff.LineOffsets (runLineOffsetParser, Position, posOffset)
import StandOff.Internalize (internalize)
import StandOff.TagSerializer
import StandOff.AttributeSerializer
import StandOff.AnnotationTypeDefs (makeAttributiveRanges, isMarkupRangeP, isRelationP, isPredicateP)
import StandOff.DomTypeDefs (XML, isXMLDeclarationP, isElementP, xmlSpanning)
import StandOff.TagTypeDefs (NSNameValueSerializer(..))
import StandOff.Owl

import StandOff.External.StandoffModeDump


-- * The commands of the @standoff@ commandline program.

type AnnotationsParser = Handle -> IO [StandoffModeRange]


-- | Formats of annotations
data AnnotationFormat = StandoffModeELisp | StandoffModeJSON
  deriving (Eq, Show)

getAnnotationsParser :: AnnotationFormat -> AnnotationsParser
getAnnotationsParser StandoffModeELisp = runELispDumpParser
getAnnotationsParser StandoffModeJSON = runJsonParser

-- | Commands and their commandline options.
data Command
  = Offsets String
  | Internalize
    { intlz_tagSrlzr :: TagSerializer
    , intlz_attrSrlzr :: AttrSerializer
    , intlz_pi :: Maybe String
    , intlz_annFormat :: AnnotationFormat
    , intlz_ann :: FilePath
    , intlz_src :: FilePath
    }
  | Owl2Csv
    { ontologyFilter :: OntologyFilter
    , csvDelimiter :: String
    , inFile :: String }
  deriving (Eq, Show)

-- | Parser for the commands of the standoff commandline program.
command_ :: Parser Command
command_ = subparser
  ( command "internalize" internalizeInfo_
    <> command "offsets" offsetsInfo_
    <> command "owl2csv" owl2csvInfo_
  )


-- * Options for the @offset@ command.

offsets_ :: Parser Command
offsets_ = Offsets <$> argument str (metavar "FILE")

offsetsInfo_ :: ParserInfo Command
offsetsInfo_ =
  (info (offsets_ <**> helper)
    (fullDesc
     <> progDesc "Returns the character offsets, lines and columns of the nodes of an XML file."
     <> header "standoff offsets - an xml parser returning node positions."))


-- * Options for the internalize command

data TagSerializer
  = Simple
  | Namespace String
  | Span String String
  | TEI
  deriving (Eq, Show)

data AttrSerializer = AttrSerializer String String
  deriving (Eq, Show)


internalize_ :: Parser Command
internalize_ = Internalize
  <$> ((Span
         <$> strOption
         (metavar "ELEMENT TYPE-ATTR"
          <> short 'n'
          <> long "span"
          <> help "Serialize tags into elements of name ELEMENT. The markup type is written to the attribute named TYPE-ATTR.")
         <*> argument str ( metavar ""))
       <|>
       (flag' TEI
         (short 't'
          <> long "tei"
          <> help "Conveniance for \"-n span rendition\". This seems to be working good for TEI P5 source files."))
       <|>
       (Namespace
         <$> strOption
         (metavar "PREFIX"
           <> short 'p'
           <> long "prefix"
           <> help "Serialize tags using the markup type as tag name. The name is prefixed with a general PREFIX, the namespace of which is defined on every internalized tag. So, using this tag internalizer every internalized tag starts like this: <PREFIX:localname xmlns:PREFIX='...' ...>. Be careful not to break namespace definitions of the xml tree in source file. Is it valid xml when the namespace, which is connected to a prefix, is changed while a so-prefixed elment is still open?"))
       <|>
       (flag' Simple
        (short 's'
          <> long "simple"
          <> help "Serialize tags with a very simple serializer that uses the markup type as tag name. This is only usefull for markup types without namespaces."))
        
      )
  <*> (AttrSerializer
       <$> strOption
        ( short 'r'
          <> long "range-id"
          <> help "Attribute name for markup range IDs. Defaults to \"rid\"."
          <> value "rid")
        <*> strOption
        ( short 'e'
          <> long "element-id"
          <> help "Attribute name for markup element IDs. Defaults to \"eid\"."
          <> value "eid"))
  <*> optional (strOption
                ( short 'i'
                  <> long "processing-instruction"
                  <> help "Insert a processing instruction into the result."
                  <> metavar "PI"))
  <*> ((flag' StandoffModeJSON
        (short 'j'
         <> long "standoff-json"
         <> help "Annotations in standoff-mode's JSON format."))
       <|>
       (flag' StandoffModeELisp
        (short 'l'
         <> long "standoff-dump"
         <> help "Annotations in standoff-mode's dump format (Emacs lisp).")))
  <*> argument str (metavar "EXTERNAL")
  <*> argument str (metavar "SOURCE")

internalizeInfo_ :: ParserInfo Command
internalizeInfo_ =
  (info (internalize_ <**> helper)
    (fullDesc
     <> progDesc "Internalize external annotations given in DUMPFILE into SOURCE. DUMPFILE must be generated (or must look like it's been generated) with GNU Emacs' standoff-mode. SOURCE must be a valid XML file, at least it must contain a root node. There are options on how the internalizer should serialize markup ranges, its type information, IDs etc. By default only markup ranges are internalized, but not relations."
     <> header "standoff internalize - internalize standoff markup into an xml file."
     <> footer "Roadmap: A serializer which takes a map of prefixes is about to be implemented."))


-- * The @owl2csv@ command.

data OntologyFilter = Ontology' | OntologyResource'
  deriving (Show, Eq)

owl2csv_ :: Parser Command
owl2csv_ = Owl2Csv
  <$> (flag' OntologyResource'
        (long "resource"
          <> short 'r'
          <> help "Parse the owl:Class, owl:ObjectProperty and owl:DatatypeProperty to ontology resources.")
       <|>
       flag OntologyResource' Ontology'
        (long "ontology"
          <> short 'o'
          <> help "Parse the whole ontology to single CSV line."))
  <*> strOption (long "csv-delimiter"
                  <> help "Delimiter for CSV output. Defaults to ',' (comma)."
                  <> value ","
                  <> metavar "CHAR")
  <*> argument str (metavar "INFILE")

owl2csvInfo_ :: ParserInfo Command
owl2csvInfo_ =
  info (helper <*> owl2csv_)
  ( fullDesc
    <> progDesc "Minimalistic conversion from OWL to CSV for standoff database."
    <> header "standoff owl2csv - Converts OWL to CSV as needed by standoff database.")


-- * The @standoff@ commandline program.

run :: Command -> IO ()
run (Offsets fileName) = do
  c <- readFile fileName
  lOffsets <- runLineOffsetParser fileName c
  nOffsets <- runXmlParser lOffsets fileName c
  print nOffsets
run (Internalize
     tagSlizer
     (AttrSerializer rangeIdAttr elementIdAttr )
     procInstr
     annFormat
     annFile
     xmlFile) = do
  annotsH <- openFile annFile ReadMode
  external <- (getAnnotationsParser annFormat) annotsH

  xmlContents <- readFile xmlFile
  lOffsets <- runLineOffsetParser xmlFile xmlContents
  xml <- runXmlParser lOffsets xmlFile xmlContents
  let internal = filter isElementP xml

  let internalzd = internalize xmlContents internal external tagSlizer'
  putStr $ postProcess xml internalzd
  where tagSlizer' = case tagSlizer of
                       Simple -> serializeTag (idAttrSlizer Nothing LocalName) 
                       Namespace prefix -> serializeNsTag (idAttrSlizer Nothing LocalName) prefix
                       Span elName typeAttr -> serializeSpanTag (idAttrSlizer (Just typeAttr) LocalName) elName
                       TEI -> serializeSpanTag (idAttrSlizer (Just "rendition") LocalName) "span"
        postProcess xml rs = insertAt rs procInstr (behindXMLDeclOrTop xml)
        insertAt s (Just new) pos = (take pos s) ++ "\n" ++ new ++ (drop (pos) s)
        insertAt s Nothing _ = s
        behindXMLDeclOrTop xml
          | length decl == 1 = (posOffset $ snd $ xmlSpanning $ head decl) - 1
          | otherwise = 0
          where decl = filter isXMLDeclarationP xml
        idAttrSlizer = (serializeAttributes (Just rangeIdAttr) (Just elementIdAttr))
run (Owl2Csv ontFilter csvDelimiter inFile) = do
  parsed <- runOwlParser inFile
  B.putStr $ Csv.encodeWith csvOpts $ map ReadOwl $ filter (predicate ontFilter) parsed
  where
    csvOpts = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral $ ord $ head csvDelimiter
      }
    predicate :: OntologyFilter -> (Ontology -> Bool)
    predicate (Ontology') = isOntology
    predicate (OntologyResource') = isOntologyResource


opts :: ParserInfo Command
opts = info
       (command_ <**> helper)
       (fullDesc <>
         header "standoff - a tool for handling standoff annotations (aka external markup)." <>
         progDesc "standoff offers commands for parsing a dump file that contains external markup and for internalizing such external markup into an xml file. There is also a command for getting the positions of the tags of an xml file." <>
         footer "See also: https://github.com/lueck/standoff-mode/ - standoff-mode is a tagger for GNU Emacs.")

main :: IO ()
main = execParser opts >>= run
