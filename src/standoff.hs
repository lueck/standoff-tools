{-# LANGUAGE CPP #-}
import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import qualified Text.Parsec as P

import StandOff.XML.NodeOffsets (xmlDocument)
import StandOff.XML.LineOffsets (lineOffsets, Position, posOffset)
import StandOff.ELisp.DumpFile (elDump)
import StandOff.Internalizer.Internalize (internalize)
import StandOff.XML.TagSerializer
import StandOff.Data.Annotation (makeAttributiveRanges)
import StandOff.Data.XML (XML, isXMLDeclarationP, isElementP, xmlSpanning)

data Serializer = Simple
  | RDF String
  | Namespace String
  deriving (Eq, Show)

data Options = Options
  { optCommand :: Command
  } deriving (Eq, Show) 

data Command = Offsets String
  | Dumped String
  | Internalize Serializer (Maybe String) String String
  deriving (Eq, Show)

offsets_ :: Parser Command
offsets_ = Offsets <$> argument str (metavar "FILE")

dumped_ :: Parser Command
dumped_ = Dumped <$> argument str (metavar "FILE")

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

run :: Command -> IO ()
run (Offsets fileName) = do
  c <- readFile fileName
  case P.runParser lineOffsets () fileName c of
    Left err -> do putStrLn "Error parsing line lengths:"
                   print err
    Right os -> case P.runParser xmlDocument os fileName c of
                  Left e -> do putStrLn "Error parsing input:"
                               print e
                  Right r -> print r
run (Dumped fileName) = do
  c <- readFile fileName
  case P.runParser elDump () fileName c of
    Left e -> do putStrLn "Error parsing elisp dump file:"
                 print e
    Right r -> print r
run (Internalize slizer procInstr dumpFile xmlFile) = do
  dumpContents <- readFile dumpFile
  case P.runParser elDump () dumpFile dumpContents of
    Left errDump -> do putStrLn "Error parsing elisp dump file:"
                       print errDump
    Right dumped -> do
      xmlContents <- readFile xmlFile
      case P.runParser lineOffsets () xmlFile xmlContents of
        Left errLines -> do putStrLn "Error parsing line lengths:"
                            print errLines
        Right lOffsets -> do
          case P.runParser xmlDocument lOffsets xmlFile xmlContents of
            Left errXml -> do putStrLn "Error parsing XML input:"
                              print errXml
            Right xml -> do
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
