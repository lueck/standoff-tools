{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.SourcePosMapping (htf_thisModulesTests) where

import Test.Framework

import qualified Data.Map as Map

import StandOff.SourcePosMapping

indexed = 0

test_emptyCornerCase = do
  let fPath = "testsuite/empty.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  assertEqual 0 $ length offsetMapping

test_emptyLinearMapCornerCase = do
  let fPath = "testsuite/empty.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  let m = lineColumnOffsetMapping offsetMapping
  assertEqual 0 $ Map.size m


test_simple = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  assertEqual 2778 $ length offsetMapping

test_simpleLinearMap = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  let m = lineColumnOffsetMapping offsetMapping
  assertEqual 2778 $ Map.size m
  assertEqual (Just 0) $ Map.lookup (lineColumnKey 1 1) m
  assertEqual (Just 0x11) $ Map.lookup (lineColumnKey 1 0x12) m
  -- the trailing newline belongs to the line it ends
  assertEqual (Just 0x12) $ Map.lookup (lineColumnKey 1 0x13) m
  -- CR belongs to the next line
  assertEqual Nothing $ Map.lookup (lineColumnKey 1 0x14) m
  assertEqual (Just 0x13) $ Map.lookup (lineColumnKey 2 1) m
  assertEqual (Just 0xace) $ Map.lookup (lineColumnKey 47 1) m
  assertEqual (Just 0xad8) $ Map.lookup (lineColumnKey 47 11) m
  assertEqual (Just 0xad9) $ Map.lookup (lineColumnKey 47 12) m
  assertEqual Nothing $ Map.lookup (lineColumnKey 47 13) m
  -- there is nothing behind the trailing newline
  assertEqual Nothing $ Map.lookup (lineColumnKey 48 1) m
  -- there is one more than the last offset in the map, because it is zero-indexed
  assertEqual (0xad9 + 1) $ Map.size m

test_advancedLinearMapTabLinearity = do
  let fPath = "testsuite/advanced.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  let m = lineColumnOffsetMapping offsetMapping
  assertEqual 238 $ Map.size m
  assertEqual (Just 0) $ Map.lookup (lineColumnKey 1 1) m
  -- testing linearity around tab character
  assertEqual (Just 0x7d) $ Map.lookup (lineColumnKey 8 1) m
  assertEqual (Just 0x7e) $ Map.lookup (lineColumnKey 8 2) m
  assertEqual (Just 0x7f) $ Map.lookup (lineColumnKey 8 3) m
  assertEqual Nothing $ Map.lookup (lineColumnKey 8 0) m
