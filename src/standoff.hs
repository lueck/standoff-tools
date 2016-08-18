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
import StandOff.Data.Annotation (makeAttributiveRanges, isMarkupRangeP, isRelationP, isPredicateP)
import StandOff.Data.XML (XML, isXMLDeclarationP, isElementP, xmlSpanning)

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
rangeAnnotations_ = flag' Ranges (short 'r' <> long "range-annotations" <> help "Parse only range annotation types dumped in the elisp dump file.")

relationAnnotations_ :: Parser AnnotationTypes
relationAnnotations_ = flag' Relations (short 'l' <> long "relation-annotations" <> help "Parse only relation annotation types dumped in the elisp dump file.")

predicateAnnotations_ :: Parser AnnotationTypes
predicateAnnotations_ = flag' Predicates (short 'r' <> long "predicate-annotations" <> help "Parse only predicate annotation types dumped in the elisp dump file.")

butRangeAnnotations_ :: Parser AnnotationTypes
butRangeAnnotations_ = flag' ButRanges (short 'R' <> long "but-range-annotations" <> help "Parse all non-range annotation types (i.e. relations and predicates) dumped in the elisp dump file.")

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
serializer_ = simpleSerializer_ <|> namespaceSerializer_ <|> rdfSerializer_

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

internalize_ :: Parser Command
internalize_ = Internalize
  <$> serializer_
  <*> optional (strOption ( short 'i'
                             <> long "processing-instruction"
                             <> help "Insert a processing instruction into the result."))
  <*> argument str (metavar "DUMPFILE")
  <*> argument str (metavar "XMLFILE")
  
command_ :: Parser Command
command_ = subparser
  ( command "offsets"
    (info offsets_
      (progDesc "Parse line offsets"))
    <> command "dumped"
    (info dumped_
      (progDesc "Parse emacs lisp dump file"))
    <> command "internalize"
    (info internalize_
      (progDesc "Internalize external markup"))
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
                    Simple -> serializeTag
                    -- FIXME: define tag serializers for attributes and options
                    RDF elName -> serializeSpanTag serializeAttributes elName
                    Namespace prefix -> serializeNsTag serializeAttributes prefix
        insertAt s (Just new) pos = (take pos s) ++ "\n" ++ new ++ (drop (pos) s)
        insertAt s Nothing _ = s
        behindXMLDeclOrTop xml
          | length decl == 1 = (posOffset $ snd $ xmlSpanning $ head decl) - 1
          | otherwise = 0
          where decl = filter isXMLDeclarationP xml

opts :: ParserInfo Command
opts = info (command_ <**> helper) idm

main :: IO ()
main = execParser opts >>= run
