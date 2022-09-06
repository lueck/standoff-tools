{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.MarkupTree (htf_thisModulesTests) where

import Test.Framework

import StandOff.XmlParsec
import StandOff.SourcePosMapping
import StandOff.MarkupTree
import Test.StandOff.TestSetup

indexed = 0

test_simpleMarkupChildren = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual 5 $ length $ getChildren $ getNthNode [0] xml
  assertEqual 2 $ length $ getMarkupChildren $ getNthNode [0] xml
  assertEqual 11 $ length $ getChildren $ getNthNode [0,3] xml
  assertEqual 5 $ length $ getMarkupChildren $ getNthNode [0,3] xml
