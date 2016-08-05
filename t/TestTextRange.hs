{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestTextRange (htf_thisModulesTests) where

import Test.Framework

import TextRange
import AnnotationData
import XMLData
import LineOffsets as L

sampleRanges = [ (MarkupRange "a1" "m1" "root" 1 100 "")
               , (MarkupRange "a2" "m2" "div" 1 20 "")
               , (MarkupRange "a3" "m3" "div" 15 40 "")
               , (MarkupRange "a4" "m4" "span" 5 7 "")
               ]

sampleElements = [ (Element "r" []
                     (L.Position 1 1 1) (L.Position 3 1 3)
                     (L.Position 96 1 96) (L.Position 100 1 100)
                     [])
                 ]

test_same = assertEqual True ((sampleRanges !! 0) <<>> (sampleRanges !! 1))
--test_mixed = assertThrowsSome ((sampleRanges !! 2) <<>> (sampleElements !! 0))
