import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as Csv

import StandOff.Owl

data Options
  = Options
    { ontologyFilter :: OntologyFilter
    , csvDelimiter :: String
    , inFile :: String
    }

data OntologyFilter = Ontology' | OntologyResource'

-- | Parser for command line options.
options_ :: Parser Options
options_ = Options
  <$> (flag' OntologyResource'
        (long "resource"
          <> short 'r'
          <> help "")
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

run :: Options -> IO ()
run (Options ontFilter csvDelimiter inFile) = do
  parsed <- runOwlParser inFile
  --putStrLn $ show ontFilter
  --print $ filter (predicate ontFilter) parsed
  --print parsed
  B.putStr $ Csv.encodeWith csvOpts $ map ReadOwl $ filter (predicate ontFilter) parsed
  where
    csvOpts = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral $ ord $ head csvDelimiter
      }
    predicate :: OntologyFilter -> (Ontology -> Bool)
    predicate (Ontology') = isOntology
    predicate (OntologyResource') = isOntologyResource

    
main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options_)
           ( fullDesc
             <> progDesc "Converts OWL to CSV as needed by standoff database."
             <> header "owlSchema2csv - Minimalistic conversion from OWL to CSV for standoff database.")
