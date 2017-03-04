{-# LANGUAGE CPP, FlexibleContexts #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import qualified Text.Parsec as P
import Data.Functor.Identity (Identity)
import Data.Aeson (encode, toJSON)
import qualified Data.ByteString.Lazy as B
import Language.Haskell.TH.Ppr (bytesToString)

import StandOff.XmlParsec (runXmlParser)
import StandOff.LineOffsets (runLineOffsetParser, Position, posOffset)
import StandOff.External.StandoffModeDump (runELispDumpParser)
import StandOff.Internalize (internalize)
import StandOff.TagSerializer
import StandOff.AttributeSerializer
import StandOff.AnnotationTypeDefs (makeAttributiveRanges, isMarkupRangeP, isRelationP, isPredicateP)
import StandOff.DomTypeDefs (XML, isXMLDeclarationP, isElementP, xmlSpanning)
import StandOff.TagTypeDefs (NSNameValueSerializer(..))

data OutputFormat = Raw
  | Json
  deriving (Eq, Show)

data AnnotationTypes = AllAnnotations
  | Ranges
  | Relations
  | Predicates
  | ButRanges
  deriving (Eq, Show)

data TagSerializer = Simple
  | Namespace String
  | Span String String
  | TEI
  deriving (Eq, Show)

data AttrSerializer = AttrSerializer String String
  deriving (Eq, Show)

data Options = Options
  { optCommand :: Command
  } deriving (Eq, Show) 

data Command = Offsets String
  | Dumped (Maybe OutputFormat) (Maybe AnnotationTypes) String
  | Internalize TagSerializer AttrSerializer (Maybe String) String String
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

attrSerializer_ :: Parser AttrSerializer
attrSerializer_ = AttrSerializer
  <$> strOption ( short 'r'
                  <> long "range-id"
                  <> help "Attribute name for markup range IDs. Defaults to \"rid\"."
                  <> value "rid"
                )
  <*> strOption ( short 'e'
                  <> long "element-id"
                  <> help "Attribute name for markup element IDs. Defaults to \"eid\"."
                  <> value "eid"
                )

tagSerializer_ :: Parser TagSerializer
tagSerializer_ = simpleSerializer_ <|> namespaceSerializer_ <|> spanSerializer_ <|> teiSerializer_

simpleSerializer_ :: Parser TagSerializer
simpleSerializer_ = flag' Simple (short 's' <> long "simple" <> help "Serialize tags with a very simple serializer that uses the markup type as tag name. This is only usefull for markup types without namespaces.")

namespaceSerializer_ :: Parser TagSerializer
namespaceSerializer_ =
  Namespace <$> strOption ( metavar "PREFIX"
                            <> short 'p'
                            <> long "prefix"
                            <> help "Serialize tags using the markup type as tag name. The name is prefixed with a general PREFIX, the namespace of which is defined on every internalized tag. So, using this tag internalizer every internalized tag starts like this: <PREFIX:localname xmlns:PREFIX='...' ...>. Be careful not to break namespace definitions of the xml tree in source file. Is it valid xml when the namespace, which is connected to a prefix, is changed while a so-prefixed elment is still open?" -- PREFIX defaults to \"adhoc\"."
                            -- <> value "adhoc"
                          )

spanSerializer_ :: Parser TagSerializer
spanSerializer_ = Span
  <$> strOption ( metavar "ELEMENT TYPE-ATTR"
                  <> short 'n'
                  <> long "span"
                  <> help "Serialize tags into elements of name ELEMENT. The markup type is written to the attribute named TYPE-ATTR. " --  ELEMENT defaults to \"span\" in the default namespace. TYPE-ATTR defaults to \"rendition\"."
                  -- <> value "span"
                )
  <*> argument str ( metavar "")

teiSerializer_ :: Parser TagSerializer
teiSerializer_ =
  flag' TEI ( short 't'
              <> long "tei"
              <> help "Conveniance for \"-n span rendition\". This seems to be working good for TEI P5 source files.")

internalize_ :: Parser Command
internalize_ = Internalize
  <$> tagSerializer_
  <*> attrSerializer_
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
       header "standoff internalize - internalize standoff markup into an xml file." <>
       footer "Roadmap: A serializer which takes a map of prefixes is about to be implemented."))
    <> command "offsets"
    (info (offsets_ <**> helper)
      (fullDesc <>
       progDesc "Returns the character offsets, lines and columns of the nodes of an XML file." <>
       header "standoff offsets - an xml parser returning node positions."))
  )

run :: Command -> IO ()
run (Offsets fileName) = do
  c <- readFile fileName
  lOffsets <- runLineOffsetParser fileName c
  nOffsets <- runXmlParser lOffsets fileName c
  print nOffsets
run (Dumped oFormat aTypes fileName) = do
  c <- readFile fileName
  annotations <- runELispDumpParser fileName c
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
run (Internalize
     tagSlizer
     (AttrSerializer rangeIdAttr elementIdAttr )
     procInstr
     dumpFile
     xmlFile) = do
  dumpContents <- readFile dumpFile
  dumped <- runELispDumpParser dumpFile dumpContents
  xmlContents <- readFile xmlFile
  lOffsets <- runLineOffsetParser xmlFile xmlContents
  xml <- runXmlParser lOffsets xmlFile xmlContents
  putStr (insertAt
           (internalize
             xmlContents
             (filter isElementP xml)
             (makeAttributiveRanges dumped)
             tagSlizer')
           procInstr
           (behindXMLDeclOrTop xml))
  where tagSlizer' = case tagSlizer of
                       Simple -> serializeTag (idAttrSlizer Nothing LocalName) 
                       Namespace prefix -> serializeNsTag (idAttrSlizer Nothing LocalName) prefix
                       Span elName typeAttr -> serializeSpanTag (idAttrSlizer (Just typeAttr) LocalName) elName
                       TEI -> serializeSpanTag (idAttrSlizer (Just "rendition") LocalName) "span" 
        insertAt s (Just new) pos = (take pos s) ++ "\n" ++ new ++ (drop (pos) s)
        insertAt s Nothing _ = s
        behindXMLDeclOrTop xml
          | length decl == 1 = (posOffset $ snd $ xmlSpanning $ head decl) - 1
          | otherwise = 0
          where decl = filter isXMLDeclarationP xml
        idAttrSlizer = (serializeAttributes (Just rangeIdAttr) (Just elementIdAttr))

opts :: ParserInfo Command
opts = info
       (command_ <**> helper)
       (fullDesc <>
         header "standoff - a tool for handling standoff annotations (aka external markup)." <>
         progDesc "standoff offers commands for parsing a dump file that contains external markup and for internalizing such external markup into an xml file. There is also a command for getting the positions of the tags of an xml file." <>
         footer "See also: https://github.com/lueck/standoff-mode/ - standoff-mode is a tagger for GNU Emacs.")

main :: IO ()
main = execParser opts >>= run
