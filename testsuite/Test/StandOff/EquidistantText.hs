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
import StandOff.OffsetMapping
import qualified StandOff.StringLike as SL
import StandOff.XTraverse
import StandOff.TextRange

indexed = 0

test_equidistantTextFromSimple = do
  let fPath = "testsuite/simple.xml"
  let txtPath = "testsuite/simple.equidist.txt"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  (_, txt) <- runWriterT (equidistantText tell ' ' xml c)
  expected <- readFile txtPath
  assertEqual (length expected) (length txt)
  assertEqual expected txt

test_equidistantTextFromAdvanced = do
  let fPath = "testsuite/advanced.xml"
  let txtPath = "testsuite/advanced.equidist.txt"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  (_, txt) <- runWriterT (equidistantText tell ' ' xml c)
  expected <- readFile txtPath
  assertEqual (length expected) (length txt)
  assertEqual expected txt
