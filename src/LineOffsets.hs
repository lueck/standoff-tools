module LineOffsets
  ( lineOffsets
  , main
  ) where
    
-- The lineOffsets parser returns the offsets of the lines fed to it
-- as a list. This list can be used to turn Parsec's SourcePos into
-- offsets.

-- λ> parse lineOffsets "" "ajsdf \n asdjf\r asf\nadsf"
-- Right [0,7,19]


import Text.Parsec

lineLens :: Parsec String () [Int]
lineLens = do
  l <- lineLen
  ls <- many $ do
    newline
    lineLen
  eof
  return $ l:ls

lineLen :: Parsec String () Int
lineLen = do
  l <- many $ noneOf "\n"
  return $ length l + 1

lineOffsets :: Parsec String () [Int]
lineOffsets = do
  ls <- lineLens
  return $ take (length ls) $ scanl (+) 0 ls

-- FIXME: no output!
main :: IO ()
main = do
  c <- getContents
  case parse lineOffsets "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> print r
