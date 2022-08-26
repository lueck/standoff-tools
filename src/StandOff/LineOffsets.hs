{-# LANGUAGE FlexibleContexts #-}
module StandOff.LineOffsets
  ( offset
  , lineOffsets
  , lineOffsets'
  , Position (..)
  , posOffset
  , runLineOffsetParser
  ) where

import Text.Parsec
import Data.List
import Control.Lens ((^?), element)
import qualified Data.Csv as Csv
import Data.Functor.Identity (Identity)


-- * Mapping a pair of line and column to character offset

-- | Get the character offset for a pair of line and column.
offset :: [Int] -> Int -> Int -> Maybe Int
offset offsets line col = fmap (+col) $ offsets ^? element (line - 1)

-- * Parser

-- | The lineOffsets parser returns the offsets of the lines fed to it
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


data Position = Position { pos_offset :: Int
                         , pos_line :: Int
                         , pos_column :: Int
                         }

instance Show Position where
  show p = "Line " ++ (show $ pos_line p) ++
           ", Column " ++ (show $ pos_column p) ++
           " (character offset " ++ (show $ pos_offset p) ++ ")"

instance Csv.ToField Position where
  toField = Csv.toField . pos_offset

posOffset :: Position -> Int
posOffset (Position o _ _) = o

lineLens :: (Stream s Identity Char) => Parsec s () [Int]
lineLens = do
  l <- lineLen
  ls <- many $ do
    newline
    lineLen
  eof
  return $ l:ls

lineLen :: (Stream s Identity Char) => Parsec s () Int
lineLen = do
  l <- many $ noneOf "\n"
  return $ length l + 1

lineOffsets :: (Stream s Identity Char) => Parsec s () [Int]
lineOffsets = do
  ls <- lineLens
  return $ init $ scanl (+) 0 ls


offsetsFromString :: Int -> String -> [Int]
offsetsFromString _ [] = []
offsetsFromString seen ('\n':xs) = (seen + 1) : (offsetsFromString (seen + 1) xs)
offsetsFromString seen (_:xs) = offsetsFromString (seen + 1) xs

lineOffsets' :: String -> [Int]
lineOffsets' s = 0 : offsetsFromString 0 s

runLineOffsetParser :: (Monad m, Stream s Identity Char) => String -> s -> m [Int]
runLineOffsetParser location contents = do
  return $ either (fail . (err++) . show) id (parse lineOffsets location contents)
  where
    err = "Error parsing line offsets (" ++ location ++ ") :"
