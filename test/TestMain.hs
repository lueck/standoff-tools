{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} TestMarkupRange
import {-@ HTF_TESTS @-} TestTextRange
import {-@ HTF_TESTS @-} TestTagSerializer

main = htfMain htf_importedTests

