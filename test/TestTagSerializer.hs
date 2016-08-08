{-# OPTIONS_GHC -F -pgmF htfpp #-}
module TestTagSerializer (htf_thisModulesTests) where

import Test.Framework

import StandOff.XML.TagSerializer

import TestSetup


test_splitOnHash = assertEqual ("http://arb.de/schema#","Konzept")
                   (splitNamespaceName $ (mRng "" "" "http://arb.de/schema#Konzept" 1 1))

test_splitOnSlash = assertEqual ("http://arb.de/schema/","Konzept")
                    (splitNamespaceName $ (mRng "" "" "http://arb.de/schema/Konzept" 1 1))
