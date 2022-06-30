{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module StandOff.StringLike
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified System.IO as Sys
import GHC.IO.Handle

-- | A unified interface for types that represent strings. We define
-- the functions we need. Function names are like for 'Text'.
class StringLike s where
  empty :: s
  append :: s -> s -> s
  singleton :: Char -> s
  pack :: String -> s
  take :: Int -> s -> s
  drop :: Int -> s -> s
  hPutStr :: Handle -> s -> IO ()


-- -- | Every instance of 'StringLike' is a 'Monoid'. TODO: remove?
-- -- Orphan instances!
-- instance (StringLike s) => Monoid s where
--   mempty = empty
--   mappend = append

-- | Make 'Text' an instance of 'StringLike'
instance StringLike T.Text where
  empty = T.empty
  append = T.append
  singleton = T.singleton
  pack = T.pack
  take = T.take
  drop = T.drop
  hPutStr h = T.hPutStr h

-- | Make 'String' an instance of 'StringLike'
instance StringLike [Char] where
  empty = []
  append = (++)
  singleton = (:[])
  pack = id
  take = L.take
  drop = L.drop
  hPutStr h = Sys.hPutStr h
