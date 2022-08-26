{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.EquidistantText (htf_thisModulesTests) where

import Test.Framework

import Data.Traversable
import Data.Foldable
import Control.Monad.Writer
import Data.Tree.Class
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import Text.Parsec.Text

import StandOff.DomTypeDefs hiding (start, end, getNode)
import StandOff.XmlParsec
import StandOff.EquidistantText
import StandOff.StringLike (StringLike)
import qualified StandOff.LineOffsets as LOFF
import qualified StandOff.StringLike as SL
import StandOff.XTraverse
import StandOff.TextRange


test_equidistantTextFromSimple = do
  let fPath = "testsuite/simple.xml"
  let txtPath = "testsuite/simple.equidist.txt"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  (_, txt) <- runWriterT (equidistantText tell ' ' xml c)
  expected <- readFile txtPath
  assertEqual (length expected) (length txt)
  assertEqual expected txt

test_equidistantTextFromAdvanced = do
  unitTestPending "advanced.xml is not extracted correctly! Parsec updates positions on \\t characters differently."
  let fPath = "testsuite/advanced.xml"
  let txtPath = "testsuite/advanced.equidist.txt"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  (_, txt) <- runWriterT (equidistantText tell ' ' xml c)
  expected <- readFile txtPath
  assertEqual (length expected) (length txt)
  assertEqual expected txt

-- | We to work around the problem of parsec and tabs, we replace
-- every tab character on the input stream with a space.
test_equidistantTextFromAdvancedTabsReplaced = do
  unitTestPending "Something still wrong here"
  let fPath = "testsuite/advanced.xml"
  let txtPath = "testsuite/advanced.equidist.txt"
  c <- BS.readFile fPath
  let c' =  T.decodeUtf8 c
  let notabs = T.map replaceTab c'
  lOffsets <- LOFF.runLineOffsetParser (show fPath) notabs
  xml <- runXmlParser lOffsets (show fPath) notabs
  (_, txt) <- runWriterT (equidistantText tell ' ' xml $ T.unpack c')
  expected <- readFile txtPath
  assertEqual (length expected) (length txt)
  assertEqual expected txt
  where
    replaceTab :: Char -> Char
    replaceTab '\t' = ' '
    replaceTab c = c
