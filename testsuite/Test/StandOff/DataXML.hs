{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.DataXML (htf_thisModulesTests) where

import Test.Framework
import Data.Tree.Class (getNode)

import StandOff.DomTypeDefs (XmlNode, XMLTrees)
import StandOff.TextRange
import StandOff.XmlParsec
import StandOff.SourcePosMapping
import StandOff.Splitting

import Test.StandOff.TestSetup

test_elementImplementsTextRange = do
  assertEqual 100 (start d)
  assertEqual 200 (end d)
  assertEqual (100, 200) (spans d)
  assertEqual ((99, 105), (194, 201)) (splitPoints d)
  assertEqual 100 (len d)
  assertThrowsSome (split FstSplit d)
  assertThrowsSome (leftSplit FstSplit d (getNode $ elm "x" 120 350 []))
  assertThrowsSome (rightSplit FstSplit d (getNode $ elm "x" 12 150 []))
  assertEqual True (d <<>> (getNode $ elm "span" 120 150 []))
  assertEqual True (d <<>> (getNode $ elm "span" 100 150 []))
  assertEqual True (d <<>> (getNode $ elm "span" 120 200 []))
  assertEqual False (d <<>> (getNode $ elm "span" 120 210 []))
  assertEqual True (d `before` (getNode $ elm "div" 210 300 []))
  assertEqual True (d `before` (getNode $ elm "div" 200 300 []))
  assertEqual False (d `before` (getNode $ elm "div" 10 30 []))
  assertEqual True (d `behind` (getNode $ elm "div" 10 30 []))
  assertEqual True (d `behind` (getNode $ elm "div" 10 100 []))
  assertEqual False (d `behind` (getNode $ elm "div" 210 300 []))
  assertEqual True (d `leftOverlaps` (getNode $ elm "div" 190 300 []))
  assertEqual False (d `leftOverlaps` (getNode $ elm "div" 90 130 []))
  assertEqual True (d `rightOverlaps` (getNode $ elm "div" 90 130 []))
  assertEqual False (d `rightOverlaps` (getNode $ elm "div" 210 300 []))
  where
    d :: XmlNode Int String String
    d = getNode $ elm "div" 100 200 ([]::XMLTrees Int String String)


mergeCase internal' s e = map spans $ merge internal' (mRng "a" "a" "a" s e)

indexed = 0

test_splitSimple = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual [(0x68, 0x72)] $ mergeCase xml 0x68 0x72
  assertEqual [(0x68, 0x72)] $ mergeCase xml 0x65 0x72
