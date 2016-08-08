{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestInternalizer (htf_thisModulesTests) where

import Test.Framework

import StandOff.Internalizer.ResolveOverlapping
import StandOff.Data.TextRange

import TestSetup

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

--test_internal = assertEqual "" (show internal)

-- test left-overlapping
test_internalizeLeftOverlapping = do
  assertEqual 2 (length resolved)
  assertEqual [(260, 270), (274, 282)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 260 282)

-- test split on right-overlapping and no-split on contain
test_internalizeRightOverlapping = do
  assertEqual 2 (length resolved)
  assertEqual [(235, 236), (241, 260)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 235 260)

-- test two splits on left- and right-overlapping
test_internalizeLeftRightOverlapping = do
  assertEqual 3 (length resolved)
  assertEqual [(235, 236), (241,270), (274, 282)] (map spans resolved)
  where resolved = merge internal (mRng "a" "a" "a" 235 282)


external = [ (mRng "a1" "m1" "root" 1 100)
           , (mRng "a2" "m2" "div" 1 20)
           , (mRng "a3" "m3" "div" 15 40)
           , (mRng "a4" "m4" "span" 5 7)
           ]

-- test makeQuasiTree
test_quasiTree = do
  assertEqual 5 (length tree)
  assertEqual [(1,100), (1,15), (5,7), (15,40), (15,20)] (map spans tree)
  where tree = makeQuasiTree external
