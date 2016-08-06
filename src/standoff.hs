import System.Environment
import System.IO
import qualified Text.Parsec as P

import XMLOffsets (xmlDocument)
import LineOffsets (lineOffsets', lineOffsets)
import DumpElParser (elDump)
import Internalize (internalize)
import TagSerializer

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("offsets", offsets_)
           , ("offsets_", offsets__)
           , ("dumped", parseElisp)
           , ("internalize", internalize_)
           ]

offsets_ :: [String] -> IO ()
offsets_ (fileName:_) = do
  c <- readFile fileName
  case P.runParser lineOffsets () fileName c of
    Left err -> do putStrLn "Error parsing line lengths:"
                   print err
    Right os -> case P.runParser xmlDocument os fileName c of
                  Left e -> do putStrLn "Error parsing input:"
                               print e
                  Right r -> print r

-- lineOffsets' is nice, but it eats up all the stack
offsets__ :: [String] -> IO ()
offsets__ (fileName:_) = do
  c <- readFile fileName
  case P.runParser xmlDocument (lineOffsets' c) fileName c of
    Left e -> do putStrLn "Error parsing XML input:"
                 print e
    Right r -> print r

parseElisp :: [String] -> IO ()
parseElisp (dumpFile:_) = do
  c <- readFile dumpFile
  case P.runParser elDump () dumpFile c of
    Left e -> do putStrLn "Error parsing elisp dump file:"
                 print e
    Right r -> print r

internalize_ :: [String] -> IO ()
internalize_ (dumpFile:xmlFile:_) = do
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
              putStr $ internalize xmlContents xml dumped (serializeSpanTag "span")


main :: IO ()
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args
