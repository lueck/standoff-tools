{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} TestMarkupRange
import {-@ HTF_TESTS @-} TestDataXML
import {-@ HTF_TESTS @-} TestTagSerializer
import {-@ HTF_TESTS @-} TestInternalizer

main = htfMain htf_importedTests

