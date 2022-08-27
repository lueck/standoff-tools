{-# LANGUAGE FlexibleContexts #-}

-- | This module provides a parser that produces a mapping of parsec's
-- 'SourcePos' to character offsets represented by 'Int'. With that
-- mapping it is possible to map parsec's position to integers no
-- matter of parsec's special handling of the tab character.

module StandOff.OffsetMapping
  ( OffsetMapping
  , parsecOffsetMapping
  , offsetFromSourcePos
  , LineColumnOffsetMapping
  , lineColumnOffsetMapping
  , lineColumnKey
  ) where

import Text.Parsec
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map

-- * Mapping Parsec's source positions to offsets

type OffsetMapping = [(Int, SourcePos)]

offset :: Stream s Identity Char => Parsec s () SourcePos
offset = do
  pos <- getPosition
  _ <- anyChar
  return pos

parsecOffsetMapping
  :: (Monad m, Stream s Identity Char) =>
     Int    -- ^ how the offsets are to be indexed, e.g. 0 for zero indexed
  -> String -- ^ the systemId of the input stream, used for error messages
  -> s      -- ^ the input stream
  -> m OffsetMapping
parsecOffsetMapping indexed location contents = do
  return $ either (fail . (err++) . show) (zip [indexed ..]) (parse (many offset) location contents)
  where
    err = "Error parsing offset mapping (" ++ location ++ ") : "


-- * Mapping utils

-- | This maps parsec's source positions to offsets. This is a
-- recursive function that can be used in a monotonous use case as
-- parsec's eating of the input stream.
offsetFromSourcePos
  :: Monad m =>
     Int -- ^ correction, i.e. relative position
  -> SourcePos -- ^ the source position to be mapped to an offset
  -> OffsetMapping -- ^ a mapping produced by 'parsecOffsetMapping'
  -> m (Int, OffsetMapping) -- ^ returns a pair of the offset and the
                            -- rest of the mappings
offsetFromSourcePos _ pos [] = fail $ "EOF while resolving positions" ++ show pos
-- when the position is identified, we return the (x:xs) as state
-- because getOffset may be called multiple times at the same position:
offsetFromSourcePos corr pos (x@(i, iPos):[])
  | iPos /= pos = fail $ "EOF-1 while resolving position " ++ show pos
  | otherwise = return (i + corr, (x:[])) -- i is the index of the last character
offsetFromSourcePos corr pos (x@(i, iPos):xs)
  | iPos /= pos = offsetFromSourcePos corr pos xs  -- recursivly search rest of mapping
  | otherwise = return $ (i + corr, (x:xs))


-- | A mapping of line-column pairs to offsets. This type can be used
-- for mapping in a non-monoton use case.
type LineColumnOffsetMapping = Map.Map Int Int

lineColumnKey :: Int -> Int -> Int
lineColumnKey l c = (l * 10^12) + c

-- | Make a mapping of line-column pairs to character offsets. The
-- line-column pairs are not Parsec's pairs with their special
-- handling of the tab character, but linear per line.
lineColumnOffsetMapping :: OffsetMapping -> LineColumnOffsetMapping
lineColumnOffsetMapping offsetMap =
  Map.fromList $ zip keys offsets
  where
    keys = map (uncurry lineColumnKey) $ zip lines newCols
    lines = map (sourceLine . snd) offsetMap
    newCols = foldl newCol [] $ map snd offsetMap
    newCol :: [Int] -> SourcePos -> [Int]
    newCol [] pos = [sourceColumn pos]
    newCol cs pos
      | sourceColumn pos == 1 = cs ++ [1]
      | otherwise = cs ++ [last cs + 1]
    offsets = map fst offsetMap
