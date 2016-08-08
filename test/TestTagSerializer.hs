{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestTagSerializer (htf_thisModulesTests) where

import Test.Framework

import StandOff.XML.TagSerializer
import StandOff.Data.Annotation

test_splitOnHash = assertEqual ("http://arb.de/schema#","Konzept")
                   (splitNamespaceName $ (MarkupRange "" "" "http://arb.de/schema#Konzept" 1 1 ""))

test_splitOnSlash = assertEqual ("http://arb.de/schema/","Konzept")
                    (splitNamespaceName $ (MarkupRange "" "" "http://arb.de/schema/Konzept" 1 1 ""))
