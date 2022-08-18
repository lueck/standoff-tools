{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Data.Char
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Tree.Class
import Data.Foldable
import Data.Traversable
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Binary as Bin
import Control.Monad.Writer
import Data.Maybe
import Numeric (showInt, showHex)

import Data.Version (showVersion)
import Paths_standoff_tools (version)

import StandOff.XmlParsec (runXmlParser)
import StandOff.LineOffsets (runLineOffsetParser, Position, posOffset)
import StandOff.Internalize (internalize)
import StandOff.DomTypeDefs hiding (Attribute)
import StandOff.Owl
import StandOff.External
import StandOff.AttributesMap
import StandOff.Tag
import StandOff.EquidistantText
import StandOff.ShrinkedText
import qualified StandOff.StringLike as SL

import StandOff.External.StandoffModeDump
import StandOff.External.GenericCsv


-- * Parse command line options

-- | A hidden \"version\" option which always aborts the program with exit code 0.
version_ :: Parser (a -> a)
version_ = abortOption (InfoMsg $ showVersion version) $ mconcat
  [ long "version"
  , help "Show the version of this program."
  , hidden ]


-- ** Parser for the annotations' input format

type AnnotationsParser = [Int] -> (BS.ByteString -> T.Text) -> Handle -> IO [GenericMarkup]

-- | Formats of annotations
data AnnotationFormat
  = StandoffModeELisp | StandoffModeJSON
  | GenericCsvStartEnd | GenericCsvLineColumn | GenericCsvLineColumnLength
  deriving (Eq, Show)

getAnnotationsParser :: AnnotationFormat -> AnnotationsParser
getAnnotationsParser StandoffModeELisp _ _ h = runELispDumpParser h >>= return . map somToGen
getAnnotationsParser StandoffModeJSON _ _ h = runJsonParser h >>= return . map genMrkp
getAnnotationsParser GenericCsvStartEnd _ dec h = runCsvParser startEndMarkup dec h >>= return . map genMrkp
getAnnotationsParser GenericCsvLineColumn offsets dec h = runCsvParser (lineColumnMarkup offsets) dec h >>=
  return . map genMrkp
getAnnotationsParser GenericCsvLineColumnLength offsets dec h = runCsvParser (lineColumnLengthMarkup offsets) dec h >>=
  return . map genMrkp

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

-- | Parser for annotation format command line options
annotationFormat_ :: Parser AnnotationFormat
annotationFormat_ =
  (flag' StandoffModeJSON
    (long "standoff-json"
      <> help "Annotations in standoff-mode's JSON format."))
  <|>
  (flag' StandoffModeELisp
    (long "standoff-dump"
      <> help "Annotations in standoff-mode's dump format (Emacs lisp)."))
  <|>
  (flag' GenericCsvStartEnd
    (long "csv-start-end"
      <> help "Annotations in CSV with referencing start character offset and end character offset. There must be columns named \"start\" and \"end\" and their values must be integers."))
  <|>
  (flag' GenericCsvLineColumn
    (long "csv-line-column"
      <> help "Annotations in CSV with referencing line/column-tuples. There must be columns named \"startline\", \"startcolumn\", \"endline\", and \"endcolumn\", and their values must be integers."))
  <|>
  (flag' GenericCsvLineColumnLength
    (long "csv-line-column-length"
      <> help "Annotations in CSV referencing the start by a line/column-tuple and giving the length of the text annotated range. There must be columns named \"line\", \"column\", and \"length\", and their values must be integers."))


readAttrsMapping :: Maybe FilePath -> IO AttributesMap
readAttrsMapping Nothing = return Map.empty
readAttrsMapping (Just fname) = do
  rc <- parseMapping fname
  case rc of
    Left err -> fail $ show err
    Right m -> return m



data GlobalOptions = GlobalOptions
  { _globOpts_input :: StreamableInput
  , _globOpts_output :: StreamableOutput
  , _globOpts_command :: Command
  }


-- | Commands and their commandline options.
data Command
  = Offsets
  | EquidistantText
    { equidist_fillChar :: Int
    }
  | ShrinkedText
    { shrinked_cfgFile :: FilePath
    , shrinked_outputMode :: ShrinkedOutputMode
    }
  | Internalize
    { intlz_tagSrlzr :: TagSerializerType
    , intlz_attrMapping :: Maybe FilePath
    , intlz_pi :: Maybe String
    , intlz_annFormat :: AnnotationFormat
    , intlz_ann :: FilePath
    }
  deriving (Eq, Show)


-- * Parsers for CLI


-- | Parser for the commands of the standoff commandline program.
command_ :: Parser Command
command_ = subparser
  ( command "internalize" internalizeInfo_
    <> command "offsets" offsetsInfo_
    <> command "equidist" equidistantInfo_
    <> command "shrink" shrinkInfo_
  )

-- * Parsers for input and output file

data StreamableInput = Stdin | InputFile FilePath
  deriving (Eq, Show)

data StreamableOutput = Stdout | OutputFile FilePath
  deriving (Eq, Show)

streamableInput_ :: Parser StreamableInput
streamableInput_ = fromMaybe Stdin <$> optional
  (InputFile <$> strOption
   (long "input"
     <> short 'i'
     <> help "The input file. If this is not given, the program reads from stdin."
     <> metavar "INFILE"))

streamableOutput_ :: Parser StreamableOutput
streamableOutput_ = fromMaybe Stdout <$> optional
  (OutputFile <$> strOption
   (long "output"
     <> short 'o'
     <> help "The output file. If this not given, the program writes to stdout."
     <> metavar "OUTFILE"))

streamableInputHandle :: StreamableInput -> IO Handle
streamableInputHandle Stdin = return stdin
streamableInputHandle (InputFile fname) = openFile fname ReadMode

streamableOutputHandle :: StreamableOutput -> IO Handle
streamableOutputHandle Stdout = return stdout
streamableOutputHandle (OutputFile fname) = openFile fname WriteMode


-- * Parsers for the integer format

data IntegerFormat = Decimal | Hex deriving (Eq, Show)

integerFormat_ =
  (flag Decimal Hex
    (long "hex"
     <> help "Hexadecimal numbers instead of decimals."))



-- * Options for the @offset@ command.

offsets_ :: Parser Command
offsets_ = pure Offsets

offsetsInfo_ :: ParserInfo Command
offsetsInfo_ =
  (info (offsets_ <**> version_ <**> helper)
    (fullDesc
     <> progDesc "Returns the character offsets, lines and columns of the nodes of an XML file."
     <> header "standoff offsets - an xml parser returning node positions."))


-- * Options for the @equidist@ command.

equidistant_ :: Parser Command
equidistant_ = EquidistantText
  <$> option auto (long "fill"
                   <> short 'f'
                   <> metavar "CODEPOINT"
                   <> help "The character used to fill/replace tags with. A code point has to be given. Defaults to 0x20 (space)."
                   <> value 0x20
                   <> showDefault)


equidistantInfo_ :: ParserInfo Command
equidistantInfo_ =
  (info (equidistant_ <**> version_ <**> helper)
    (fullDesc
     <> progDesc "Generates equidistant plain text from an XML input file."
     <> header "standoff equidist - generate equidistant text."))


-- * Options for the @shrink@ command.

shrinkInfo_ :: ParserInfo Command
shrinkInfo_ =
  (info (shrink_ <**> version_ <**> helper)
    (fullDesc
     <> progDesc "Generates shrinked plain text from an XML input file. If no mapping of tags to replacement characters is given, the default mapping will replace every character inside a tag or processing instruction with the empty string."
     <> header "standoff shrink - generate shrinked text."))

shrink_ :: Parser Command
shrink_ = ShrinkedText
  <$> strOption
  (long "config"
    <> metavar "CONFIG_FILE"
    <> help "Shrinking configuration in yaml format.")
  <*> shrinkedOutputMode_


data ShrinkedOutputMode
  = ShrinkedOffsetMapping
    { shrinked_offsetsOut :: FilePath
    }
  | ShrinkedSingleCSV
    { shrinkedSingle_integerFormat :: IntegerFormat
    , shrinkedSingle_newlineRepl :: Char
    }
  deriving (Eq, Show)


shrinkedOutputMode_ :: Parser ShrinkedOutputMode
shrinkedOutputMode_ =
  fromMaybe defaultFormat <$> optional
  (ShrinkedOffsetMapping <$> strOption
    (long "offsets"
     <> short 'f'
     <> help "Output the offset mapping into an extra file."
     <> metavar "OFFSET_MAPPING"))
  <|>
  (flag' defaultFormat
    (long "csv"
     <> short 'c'
     <> help "Output the offset mapping and the shrinked text as CSV. (Default)")
    *> (ShrinkedSingleCSV
        <$> integerFormat_
        <*> ((head . (<> "!")) <$> strOption -- ++"!" : assert that 'head' does not fail
             (long "newline-replacement"
              <> help "Replace newline characters with this one."
              <> value ['\n']
              <> metavar "CHARACTER"))))
  where
    defaultFormat = (ShrinkedSingleCSV Decimal '\n')


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
  <*> annotationFormat_
  <*> argument str (metavar "EXTERNAL")

internalizeInfo_ :: ParserInfo Command
internalizeInfo_ =
  (info (internalize_ <**> version_ <**> helper)
    (fullDesc
     <> progDesc "Internalize external annotations given in EXTERNAL into an XML input file.  EXTERNAL can have different formats.  The MAPPING file controls how the annotated features are serialized to XML."
     <> header "standoff internalize - internalize standoff markup into an xml file."))




printCSV :: (SL.StringLike s, Show s) => XmlNode s s -> IO ()
printCSV xml =  BL.putStr $ Csv.encodeByNameWith (csvEncodeOptions {Csv.encIncludeHeader = False}) positionHeader [xml]

csvEncodeOptions :: Csv.EncodeOptions
csvEncodeOptions = Csv.defaultEncodeOptions


-- * The @standoff@ commandline program.

run :: GlobalOptions -> IO ()
run (GlobalOptions input output Offsets) = do
  inputH <- streamableInputHandle input
  outputH <- streamableOutputHandle output
  c <- hGetContents inputH
  lOffsets <- runLineOffsetParser (show inputH) c
  nOffsets <- runXmlParser lOffsets (show inputH) c
  BL.hPutStr outputH $ Csv.encodeByNameWith csvEncodeOptions positionHeader ([]::[XmlNode String String])
  mapM_ (traverse_ printCSV ) nOffsets
run (GlobalOptions input output (EquidistantText fillChar)) = do
  inputH <- streamableInputHandle input
  outputH <- streamableOutputHandle output
  c <- hGetContents inputH
  lOffsets <- runLineOffsetParser (show inputH) c
  xml <- runXmlParser lOffsets (show inputH) c
  putChar $ chr fillChar
  s <- equidistantText putStr (chr fillChar) xml c
  return ()
run (GlobalOptions input output (ShrinkedText cfgFile (ShrinkedOffsetMapping offsetOut))) = do
  shrinkingCfg <- BL.readFile cfgFile >>=
    mkShrinkingNodeConfig (const (Right . T.unpack)) (Right . T.unpack)
  inputH <- streamableInputHandle input
  outputH <- streamableOutputHandle output
  c <- hGetContents inputH
  lOffsets <- runLineOffsetParser (show inputH) c
  xml <- runXmlParser lOffsets (show inputH) c
  offsets <- shrinkedText (hPutStr outputH) shrinkingCfg xml c
  BL.writeFile offsetOut $ foldl (<>) "" $ map Bin.encode offsets
  return ()
run (GlobalOptions input output (ShrinkedText cfgFile (ShrinkedSingleCSV integerFormat newlineRepl))) = do
  shrinkingCfg <- BL.readFile cfgFile >>=
    mkShrinkingNodeConfig (const (Right . T.unpack)) (Right . T.unpack)
  inputH <- streamableInputHandle input
  outputH <- streamableOutputHandle output
  c <- hGetContents inputH
  lOffsets <- runLineOffsetParser (show inputH) c
  xml <- runXmlParser lOffsets (show inputH) c
  (offsets, txt) <- runWriterT (shrinkedText tell shrinkingCfg xml c)
  BL.hPut outputH $ Csv.encode $
    zip3 (map formatInt offsets) (map replaceNewlines $ SL.unpack txt) (map formatInt ([1 ..] :: [Int]))
  return ()
  where
    replaceNewlines '\n' = newlineRepl
    replaceNewlines c = c
    formatInt i
      | integerFormat == Hex = showHex i ""
      | otherwise = showInt i ""
run (GlobalOptions input output
     (Internalize
     tagSlizer
     mappingFile
     procInstr
     annFormat
     annFile)) = do
  attrsMapping <- readAttrsMapping mappingFile
  let tagSlizer' = ((getTagSerializer tagSlizer) (mapExternal attrsMapping))

  inputH <- streamableInputHandle input
  outputH <- streamableOutputHandle output
  xmlContents <- hGetContents inputH
  lOffsets <- runLineOffsetParser (show inputH) xmlContents
  xml <- runXmlParser lOffsets (show inputH) xmlContents
  let internal = xml --filter isElementP xml  -- FIXME: do we have to filter?

  annotsH <- openFile annFile ReadMode
  external <- (getAnnotationsParser annFormat) lOffsets decodeUtf8 annotsH

  let internalzd = internalize xmlContents internal external tagSlizer'
  hPutStr outputH internalzd -- $ postProcess xml internalzd
  -- FIXME: postProcess again
  where
    postProcess x rs = insertAt rs procInstr (behindXMLDeclOrTop x)
    insertAt s (Just new) pos = (take pos s) ++ "\n" ++ new ++ (drop (pos) s)
    insertAt s Nothing _ = s
    behindXMLDeclOrTop x
      | length decl == 1 = (posOffset $ snd $ xmlSpanning $ head decl) - 1
      | otherwise = 0
      where decl = filter isXMLDeclarationP x


globalOpts_ :: Parser GlobalOptions
globalOpts_ = GlobalOptions <$> streamableInput_ <*> streamableOutput_ <*> command_

opts :: ParserInfo GlobalOptions
opts = info
       (globalOpts_ <**> version_ <**> helper)
       (fullDesc <>
         header "standoff - a tool for handling standoff annotations (aka external markup)." <>
         progDesc "standoff offers commands for parsing a dump file that contains external markup and for internalizing such external markup into an xml file. There is also a command for getting the positions of the tags of an xml file." <>
         footer "See also: https://github.com/lueck/standoff-mode/ - standoff-mode is a tagger for GNU Emacs.")

main :: IO ()
main = execParser opts >>= run
