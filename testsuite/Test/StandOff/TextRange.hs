{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.TextRange (htf_thisModulesTests) where

import Test.Framework

import StandOff.TextRange
import StandOff.DomTypeDefs

import Test.StandOff.TestSetup


internal = [ (elm "root" 1 1000
              [ (elm "head" 8 99 [])
              , (elm "body" 100 993
                 [ (elm "div1" 106 199
                   [])
                 , (elm "div2" 200 299
                   [ (elm "s1" 230 240 [])
                   , (elm "s2" 270 289
                      [ (elm "b" 275 282 [])
                      ])
                   ])
                 , (elm "div3" 300 399
                   [])
                 , (elm "div4" 400 987
                   [])
                 ])
              ])
           ]

-- test left-overlapping
test_mergeLeftOverlapping = do
  assertEqual 2 (length resolved)
  assertEqual [(260, 269), (274, 282)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 260 282)

-- test split on right-overlapping and no-split on contain
test_mergeRightOverlapping = do
  assertEqual 2 (length resolved)
  assertEqual [(235, 235), (241, 260)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 235 260)

-- test two splits on left- and right-overlapping
test_mergeLeftRightOverlapping = do
  assertEqual 3 (length resolved)
  assertEqual [(235, 235), (241,269), (274, 282)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 235 282)

-- test lost opening tag
test_mergeLostOpeningTag = do
  assertEqual [(234, 235)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 233 240)

-- test lost closing tag
test_mergeLostClosingTag = do
  assertEqual [(234, 235)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 230 235)

-- test in opening tag
test_mergeInOpeningTag = do
  assertEqual [(234, 235)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 231 240)

-- test in closing tag
test_mergeInClosingTag = do
  assertEqual [(234, 235)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 230 239)

-- test in both tags
test_mergeInBothTags = do
  assertEqual [(234, 235)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 231 239)

-- test merge into region without markup
test_mergePlain = do
  assertEqual [(210,220)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 210 220)

-- test merge containing
test_mergeContaining = do
  assertEqual [(220,250)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 220 250)

-- test merge with equal spans
test_mergeSpanEqual = do
  assertEqual [(230,240)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 230 240)

-- test merge contained (same as plain?)
test_mergeContained = do
  assertEqual [(150,160)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 150 160)

-- test merge behind
test_mergeBehind = do
  assertEqual [(2100,2200)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 2100 2200)

-- test merge before
test_mergeBefore = do
  assertEqual [(0,0)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 0 0)

-- test merge before at
test_mergeNextBefore = do
  assertEqual [(0,1)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 0 1)

-- test end extends into open tag
test_mergeEndIntoOpening = do
  assertEqual [(210,229)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 210 231)

-- test end extends into closing tag (same as test_mergeInClosingTag)
test_mergeEndIntoClosing = do
  assertEqual [(234,235)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 234 239)

-- test start extends into open tag (same as test_mergeInOpeningTag)
test_mergeStartIntoOpening = do
  assertEqual [(234,235)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 231 235)

-- test start extends into closing tag
test_mergeStartIntoClosing = do
  assertEqual [(241,245)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 239 245)

-- test markup in a single opening tag
test_mergeInSingleOpening = do
  assertEqual [] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 231 232)

-- test markup in a single closing tag
test_mergeInSingleClosing = do
  assertEqual [] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 238 239)

-- test markup between to elements, extending into both of them
test_mergeExtendsIntoClosingOpening = do
  assertEqual [(241,269)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 239 271)


-- corner case: test markup in a single closing tag
test_mergeIntoPlaintext = do
  assertEqual [(10, 20)] (map spans resolved)
  where resolved = merge ([]::[XML]) (mRng "a" "a" "a" 10 20)


external = [ (mRng "a1" "m1" "root" 1 100)
           , (mRng "a2" "m2" "div" 1 20)
           , (mRng "a3" "m3" "div" 15 40)
           , (mRng "a4" "m4" "span" 5 7)
           ]

-- test splitExternal, formerly known as makeQuasiTree
test_splitExternal = do
  assertEqual 5 (length tree)
  assertEqual [(1,100), (1,15), (5,7), (15,40), (15,20)] (map spans tree)
  where tree = splitExternal external
