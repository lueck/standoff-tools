{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.DataXML (htf_thisModulesTests) where

import Test.Framework

import StandOff.TextRange

import Test.StandOff.TestSetup

test_elementImplementsTextRange = do
  assertEqual 100 (start d)
  assertEqual 200 (end d)
  assertEqual (100, 200) (spans d)
  assertEqual ((100, 104), (195, 200)) (splitPoints d)
  assertEqual 100 (len d)
  assertThrowsSome (split d)
  assertThrowsSome (leftSplit d (elm "x" 120 350 []))
  assertThrowsSome (rightSplit d (elm "x" 12 150 []))
  assertEqual True (d <<>> (elm "span" 120 150 []))
  assertEqual True (d <<>> (elm "span" 100 150 []))
  assertEqual True (d <<>> (elm "span" 120 200 []))
  assertEqual False (d <<>> (elm "span" 120 210 []))
  assertEqual True (d `before` (elm "div" 210 300 []))
  assertEqual True (d `before` (elm "div" 200 300 []))
  assertEqual False (d `before` (elm "div" 10 30 []))
  assertEqual True (d `behind` (elm "div" 10 30 []))
  assertEqual True (d `behind` (elm "div" 10 100 []))
  assertEqual False (d `behind` (elm "div" 210 300 []))
  assertEqual True (d `leftOverlaps` (elm "div" 190 300 []))
  assertEqual False (d `leftOverlaps` (elm "div" 90 130 []))
  assertEqual True (d `rightOverlaps` (elm "div" 90 130 []))
  assertEqual False (d `rightOverlaps` (elm "div" 210 300 []))
  where d = elm "div" 100 200 []
