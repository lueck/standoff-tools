{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Test.StandOff.MarkupRange
import {-@ HTF_TESTS @-} Test.StandOff.DataXML
import {-@ HTF_TESTS @-} Test.StandOff.TagSerializer
import {-@ HTF_TESTS @-} Test.StandOff.TextRange
import {-@ HTF_TESTS @-} Test.StandOff.External.StandoffModeDump

main = htfMain htf_importedTests

