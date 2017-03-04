{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.TagSerializer (htf_thisModulesTests) where

import Test.Framework

import StandOff.TagSerializer

import Test.StandOff.TestSetup


test_splitOnHash = assertEqual ("http://arb.de/schema#","Konzept")
                   (splitNamespaceName "http://arb.de/schema#Konzept")

test_splitOnSlash = assertEqual ("http://arb.de/schema/","Konzept")
                    (splitNamespaceName "http://arb.de/schema/Konzept")
