{-# LANGUAGE CPP #-}
import System.IO
import Options.Applicative
import qualified Text.Parsec as P

import StandOff.XML.NodeOffsets (xmlDocument)
import StandOff.XML.LineOffsets (lineOffsets', lineOffsets)
import StandOff.ELisp.DumpFile (elDump)
import StandOff.Internalizer.Internalize (internalize)
import StandOff.XML.TagSerializer
import StandOff.Data.Annotation (isMarkupRangeP)
import StandOff.Data.XML (isElementP)

data Serializer = Simple | RDF | AnnoNamespace

data Options = Options
  { optCommand :: Command
  } deriving (Eq, Show) 

data Command = Offsets String
  | Dumped String
  | Internalize { dumpFile :: String
                , xmlFile :: String
                --, serializer :: (TagType -> Annotation -> String)
                } deriving (Eq, Show)

offsets_ :: Parser Command
offsets_ = Offsets <$> argument str (metavar "FILE")

dumped_ :: Parser Command
dumped_ = Dumped <$> argument str (metavar "FILE")

-- simpleSerializer :: Parser (TagType -> Annotation -> String)
-- simpleSerializer = 

internalize_ :: Parser Command
internalize_ = Internalize
  <$> argument str (metavar "DUMPFILE")
  <*> argument str (metavar "XMLFILE")
  -- <$> simpleSerializer <|> namespaceSerializer <|> rdfSerializer
  
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
run (Internalize dumpFile xmlFile) = do
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
              putStr (internalize
                       xmlContents
                       (filter isElementP xml)
                       (filter isMarkupRangeP dumped)
                       serializeTag )

opts :: ParserInfo Command
opts = info (command_ <**> helper) idm

main :: IO ()
main = execParser opts >>= run
