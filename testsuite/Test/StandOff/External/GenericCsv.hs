{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.External.GenericCsv (htf_thisModulesTests) where

import Test.Framework
import System.IO
import Data.Text.Encoding (decodeUtf8)

import StandOff.External.GenericCsv
import StandOff.TextRange


test_parseCSV_simple_endLeftForbidden = do
  anh <- openFile "testsuite/annotations/simple.end-left-forbidden.csv" ReadMode
  ans <- runCsvParser startEndMarkup decodeUtf8 anh
  assertBool $ (>=2) $ length ans
  assertEqual 0xa $ ncsv_start $ head ans
  assertEqual 10 $ ncsv_start $ head ans
  assertEqual 0x1f $ ncsv_end $ head ans
  -- we have a TextRange:
  assertEqual (0xa, 0x1f) $ spans $ head ans
