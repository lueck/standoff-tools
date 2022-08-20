{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.EquidistantText (htf_thisModulesTests) where

import Test.Framework

import Data.Traversable
import Data.Foldable
import Control.Monad.Writer
import Data.Tree.Class

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
