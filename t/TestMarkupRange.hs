{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestMarkupRange (htf_thisModulesTests) where

import Test.Framework

import AnnotationData
import TextRange

sampleRanges = [(MarkupRange "a1" "m1" "root" 1 100 "")
               , (MarkupRange "a2" "m2" "div" 1 20 "")
               , (MarkupRange "a3" "m3" "div" 15 40 "")
               , (MarkupRange "a4" "m4" "span" 5 7 "")
               ]

test_start = assertEqual 1 (start (sampleRanges !! 0))
test_end = assertEqual 100 (end (sampleRanges !! 0))
test_contains = assertEqual ((sampleRanges !! 0) <<>> (sampleRanges !! 1)) True
test_notContains = assertEqual ((sampleRanges !! 1) <<>> (sampleRanges !! 2)) False
test_before = assertEqual ((sampleRanges !! 3) <>< (sampleRanges !! 2)) True
test_notBefore = assertEqual ((sampleRanges !! 1) <>< (sampleRanges !! 3)) False
test_behind = assertEqual ((sampleRanges !! 2) ><> (sampleRanges !! 3)) True
test_notBehind = assertEqual ((sampleRanges !! 2) ><> (sampleRanges !! 1)) False
test_lenght = assertEqual (len $ sampleRanges !! 3) 2
test_spans = assertEqual (spans $ sampleRanges !! 3) (5, 7)
test_leftOverlaps = assertEqual ((sampleRanges !! 1) <-< (sampleRanges !! 2)) True
test_notLeftOverlaps = assertEqual ((sampleRanges !! 1) <-< (sampleRanges !! 3)) False
test_rightOverlaps = assertEqual ((sampleRanges !! 2) >-> (sampleRanges !! 1)) True
test_notRightOverlaps = assertEqual ((sampleRanges !! 1) >-> (sampleRanges !! 3)) False

test_sort = assertEqual ["a1", "a2", "a4", "a3"] (map rangeRangeId $ sortTextRanges sampleRanges)
