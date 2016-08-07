{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} TestMarkupRange
import {-@ HTF_TESTS @-} TestTextRange

main = htfMain htf_importedTests

