import System.Environment
import System.Directory
import System.IO
import Data.List

import XMLOffsets (xmlDocument)
import qualified XMLData
import LineOffsets (lineOffsets')
import DumpElParser (elDump)
import qualified Text.Parsec as P

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("offsets", offsets_)
           , ("dumped", parseElisp)
           ]

offsets_ :: [String] -> IO ()
offsets_ (fileName:_) = do
  c <- readFile fileName
  case P.runParser xmlDocument (lineOffsets' c) fileName c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> print r

parseElisp :: [String] -> IO ()
parseElisp (dumpFile:_) = do
  c <- readFile dumpFile
  case P.runParser elDump () dumpFile c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> print r


main :: IO ()
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args
