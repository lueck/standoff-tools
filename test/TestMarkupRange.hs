{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestMarkupRange (htf_thisModulesTests) where

import Test.Framework

import StandOff.Data.Annotation
import StandOff.Data.TextRange

import TestSetup

sampleRanges = [ (mRng "a1" "m1" "root" 1 100)
               , (mRng "a2" "m2" "div" 1 20)
               , (mRng "a3" "m3" "div" 15 40)
               , (mRng "a4" "m4" "span" 5 7)
               ]

test_start = assertEqual 1 (start (sampleRanges !! 0))
test_end = assertEqual 100 (end (sampleRanges !! 0))
test_contains = assertEqual ((sampleRanges !! 0) <<>> (sampleRanges !! 1)) True
test_notContains = assertEqual ((sampleRanges !! 1) <<>> (sampleRanges !! 2)) False
test_before = assertEqual ((sampleRanges !! 3) `before` (sampleRanges !! 2)) True
test_notBefore = assertEqual ((sampleRanges !! 1) `before` (sampleRanges !! 3)) False
test_behind = assertEqual ((sampleRanges !! 2) `behind` (sampleRanges !! 3)) True
test_notBehind = assertEqual ((sampleRanges !! 2) `behind` (sampleRanges !! 1)) False
test_lenght = assertEqual (len $ sampleRanges !! 3) 2
test_spans = assertEqual (spans $ sampleRanges !! 3) (5, 7)
test_leftOverlaps = assertEqual ((sampleRanges !! 1) `leftOverlaps` (sampleRanges !! 2)) True
test_notLeftOverlaps = assertEqual ((sampleRanges !! 1) `leftOverlaps` (sampleRanges !! 3)) False
test_rightOverlaps = assertEqual ((sampleRanges !! 2) `rightOverlaps` (sampleRanges !! 1)) True
test_notRightOverlaps = assertEqual ((sampleRanges !! 1) `rightOverlaps` (sampleRanges !! 3)) False

test_sort = assertEqual ["a1", "a2", "a4", "a3"] (map rangeRangeId $ sortTextRanges sampleRanges)

test_split = do
  assertEqual (1, 15) (spans fstPart)
  assertEqual (15, 20) (spans sndPart)
  assertEqual False (end fstPart > start sndPart)
  assertEqual False (fstPart `leftOverlaps` sndPart)
  assertEqual False (fstPart `rightOverlaps` sndPart)
  where fstPart = fst splits
        sndPart = snd splits
        splits = leftSplit (sampleRanges !! 1) (sampleRanges !! 2)