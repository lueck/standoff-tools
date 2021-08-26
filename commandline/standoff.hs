import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Data.Char
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import StandOff.XmlParsec (runXmlParser)
import StandOff.LineOffsets (runLineOffsetParser, Position, posOffset)
import StandOff.Internalize (internalize)
import StandOff.DomTypeDefs (XML, isXMLDeclarationP, isElementP, xmlSpanning)
import StandOff.Owl
import StandOff.External
import StandOff.AttributesMap
import StandOff.Tag

import StandOff.External.StandoffModeDump
import StandOff.External.GenericCsv

-- * The commands of the @standoff@ commandline program.

type AnnotationsParser = (BS.ByteString -> T.Text) -> Handle -> IO [GenericMarkup]


-- | Formats of annotations
data AnnotationFormat = StandoffModeELisp | StandoffModeJSON | GenericCsv
  deriving (Eq, Show)

getAnnotationsParser :: AnnotationFormat -> AnnotationsParser
getAnnotationsParser StandoffModeELisp _ h = do { ms <- runELispDumpParser h; return $ map somToGen ms }
getAnnotationsParser StandoffModeJSON _ h = do { ms <- runJsonParser h; return $ map genMrkp ms }
getAnnotationsParser GenericCsv dec h = do { ms <- runCsvParser startEndMarkup dec h; return $ map genMrkp ms }

somToGen :: StandoffModeRange -> GenericMarkup
somToGen = genMrkp

data TagSerializerType
  = ConstTagSerializer String
  | VarTagSerializer String String
  deriving (Eq, Show)

getTagSerializer :: (ToAttributes a, IdentifiableSplit a) =>
                    TagSerializerType -> ((ExternalAttributes -> [Attribute]) -> TagSerializer a)
getTagSerializer (ConstTagSerializer el) = constTagSerializer el
getTagSerializer (VarTagSerializer attr el) = fail "This serializer is still undefined"


readAttrsMapping :: Maybe FilePath -> IO AttributesMap
readAttrsMapping Nothing = return Map.empty
readAttrsMapping (Just fname) = do
  rc <- parseMapping fname
  case rc of
    Left err -> fail $ show err
    Right m -> return m

-- | Commands and their commandline options.
data Command
  = Offsets String
  | Internalize
    { intlz_tagSrlzr :: TagSerializerType
    , intlz_attrMapping :: Maybe FilePath
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

internalize_ :: Parser Command
internalize_ = Internalize
  <$> ((ConstTagSerializer <$>
         strOption (
           metavar "ELEMENT"
           <> short 'c'
           <> long "const"
           <> help "Serialize tags into elements of name ELEMENT."))
       <|>
       (VarTagSerializer
         <$> strOption (
           metavar "ATTRIBUTE FALLBACK"
           <> short 'v'
           <> long "variable"
           <> help "Serialize tags setting the tag name variably from the tag's feature named ATTRIBUTE. If ATTRIBUTE is not among the tag's features, FALLBACK is used as tag name.")
         <*> argument str (metavar "")))
  <*> optional (strOption
                (short 'm'
                 <> long "mapping"
                 <> help "A mapping file of the external markup's features to tag attributes that will be internalized into the source. If not mapping is given, an empty mapping is used, which means, that no attributes are serialized."
                 <> metavar "MAPPING"))
  <*> optional (strOption
                (short 'i'
                 <> long "processing-instruction"
                 <> help "Insert a processing instruction into the result."))
  <*> ((flag' StandoffModeJSON
        (short 'j'
         <> long "standoff-json"
         <> help "Annotations in standoff-mode's JSON format."))
       <|>
       (flag' StandoffModeELisp
        (short 'l'
         <> long "standoff-dump"
         <> help "Annotations in standoff-mode's dump format (Emacs lisp)."))
       <|>
       (flag' GenericCsv
        (short 'c'
         <> long "csv"
         <> help "Annotations in CSV.")))
  <*> argument str (metavar "EXTERNAL")
  <*> argument str (metavar "SOURCE")

internalizeInfo_ :: ParserInfo Command
internalizeInfo_ =
  (info (internalize_ <**> helper)
    (fullDesc
     <> progDesc "Internalize external annotations given in EXTERNAL into SOURCE.  SOURCE must be a valid XML file, at least it must contain a root node.  EXTERNAL can have different formats.  The MAPPING file controls how the annotated features are serialized to XML."
     <> header "standoff internalize - internalize standoff markup into an xml file."))


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
     mappingFile
     procInstr
     annFormat
     annFile
     xmlFile) = do
  annotsH <- openFile annFile ReadMode
  external <- (getAnnotationsParser annFormat) decodeUtf8 annotsH

  attrsMapping <- readAttrsMapping mappingFile
  let tagSlizer' = ((getTagSerializer tagSlizer) (mapExternal attrsMapping))

  xmlContents <- readFile xmlFile
  lOffsets <- runLineOffsetParser xmlFile xmlContents
  xml <- runXmlParser lOffsets xmlFile xmlContents
  let internal = filter isElementP xml

  let internalzd = internalize xmlContents internal external tagSlizer'
  putStr $ postProcess xml internalzd
  where
    postProcess x rs = insertAt rs procInstr (behindXMLDeclOrTop x)
    insertAt s (Just new) pos = (take pos s) ++ "\n" ++ new ++ (drop (pos) s)
    insertAt s Nothing _ = s
    behindXMLDeclOrTop x
      | length decl == 1 = (posOffset $ snd $ xmlSpanning $ head decl) - 1
      | otherwise = 0
      where decl = filter isXMLDeclarationP x
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
