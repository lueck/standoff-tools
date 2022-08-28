{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Test.StandOff.MarkupRange
import {-@ HTF_TESTS @-} Test.StandOff.DataXML
import {-@ HTF_TESTS @-} Test.StandOff.TagSerializer
import {-@ HTF_TESTS @-} Test.StandOff.TextRange
import {-@ HTF_TESTS @-} Test.StandOff.XmlParsec
import {-@ HTF_TESTS @-} Test.StandOff.EquidistantText
import {-@ HTF_TESTS @-} Test.StandOff.SourcePosMapping
import {-@ HTF_TESTS @-} Test.StandOff.MarkupTree
-- import {-@ HTF_TESTS @-} Test.StandOff.External.StandoffModeDump

main = htfMain htf_importedTests

