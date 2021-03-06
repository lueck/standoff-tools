module StandOff.LineOffsets
  ( lineOffsets
  , lineOffsets'
  , getOffset
  , Position (..)
  , posOffset
  , runLineOffsetParser
  ) where
    
-- The lineOffsets parser returns the offsets of the lines fed to it
-- as a list. This list can be used to turn Parsec's SourcePos into
-- offsets.

-- lineOffsets' is a function that takes [Char] and returns a list of
-- line offsets.

-- Either of these may be used as user state of a parser, that needs
-- offset positions, e.g.:
-- runParser myParser (lineOffsets' s) filePath s

-- offset can be used in a parser that has a list of line offsets set
-- as user state. Usage: somePos <- offset

-- λ> parse lineOffsets "" "ajsdf \n asdjf\r asf\nadsf"
-- Right [0,7,19]


import Text.Parsec

data Position = Position { offset :: Int
                         , line :: Int
                         , column :: Int
                         } deriving (Show)

posOffset :: Position -> Int
posOffset (Position o _ _) = o

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
  return $ init $ scanl (+) 0 ls


getOffset :: Int -> Parsec String [Int] Position
getOffset cor = do
  offsets <- getState
  pos <- getPosition
  return $ Position { line = sourceLine pos
                    , column = sourceColumn pos
                    -- parsec's column in SourcePos starts with 1, but
                    -- we say that the first char in a file has offset
                    -- of 0. So we decrement the offset by 1.
                    , offset = (offsets !! ((sourceLine pos)-1)) + (sourceColumn pos) + cor - 1
                    }


offsetsFromString :: Int -> String -> [Int]
offsetsFromString _ [] = []
offsetsFromString seen ('\n':xs) = (seen + 1) : (offsetsFromString (seen + 1) xs)
offsetsFromString seen (_:xs) = offsetsFromString (seen + 1) xs

lineOffsets' :: String -> [Int]
lineOffsets' s = 0 : offsetsFromString 0 s

runLineOffsetParser :: String -> String -> IO [Int]
runLineOffsetParser location contents = do
  return $ either (fail . (err++) . show) id (parse lineOffsets location contents)
  where
    err = "Error parsing line offsets (" ++ location ++ ") :"
