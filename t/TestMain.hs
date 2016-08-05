{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
--import {-@ HTF_TESTS @-} TestInternalizer
import {-@ HTF_TESTS @-} TestMarkupRange

main = htfMain htf_importedTests

