{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.StandOff.ShrinkedText (htf_thisModulesTests) where

import Test.Framework
import GHC.Stack (HasCallStack)
import Control.Monad.Writer
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.IO
import qualified Data.Binary as Bin
import Data.Char

import StandOff.ShrinkedText
import StandOff.DomTypeDefs
import StandOff.SourcePosMapping hiding (OffsetMapping)
import StandOff.XmlParsec

import Test.StandOff.TestSetup


indexed = 0


-- * Helper funtions for running tests

-- | Validate that the produced shrinked text for BASE xml equals to
-- the shrinked text given in
-- testsuite/annotations-shrinked/BASE.shrinked.txt.
validateShrinkedTXT :: HasCallStack => String -> IO ()
validateShrinkedTXT base = do
  let xmlFile = "testsuite/" ++ base ++ ".xml"
  let shrinkedTxtPath = "testsuite/annotations-shrinked/" ++ base ++ ".shrinked.txt"
  let cfgFile = "testsuite/annotations-shrinked/shrink.yaml"
  expected <- readFile shrinkedTxtPath
  shrinkingCfg <- BL.readFile cfgFile >>=
    mkShrinkingNodeConfig (const (Right . T.unpack)) (Right . T.unpack)
  c <- readFile xmlFile
  offsetMapping <- parsecOffsetMapping indexed xmlFile c
  xml <- runXmlParser offsetMapping xmlFile c
  (_offsets, txt) <- runWriterT (shrinkedText tell shrinkingCfg xml c)
  assertEqual expected txt

-- | Validate that the produced offset mapping for BASE xml equals to
-- the offset mapping given in
-- testsuite/annotations-shrinked/BASE.offsets.dat.
validateOffsetMapping :: HasCallStack => String -> IO ()
validateOffsetMapping base = do
  let xmlFile = "testsuite/" ++ base ++ ".xml"
  let offsetsPath = "testsuite/annotations-shrinked/" ++ base ++ ".offsets.dat"
  let cfgFile = "testsuite/annotations-shrinked/shrink.yaml"
  expected <- BL.readFile offsetsPath
  shrinkingCfg <- BL.readFile cfgFile >>=
    mkShrinkingNodeConfig (const (Right . T.unpack)) (Right . T.unpack)
  c <- readFile xmlFile
  offsetMapping <- parsecOffsetMapping indexed xmlFile c
  xml <- runXmlParser offsetMapping xmlFile c
  (offsets, _txt) <- runWriterT (shrinkedText tell shrinkingCfg xml c)
  assertEqual (Bin.decode expected) offsets

-- | Validate that all characters' offsets in the given shrinked text
-- are mapped correctly to offsets of the corresponding characters in
-- the BASE xml file.
validateMappedCharacters :: HasCallStack => String -> IO ()
validateMappedCharacters base = do
  let xmlFile = "testsuite/" ++ base ++ ".xml"
  let shrinkedTxtPath = "testsuite/annotations-shrinked/" ++ base ++ ".shrinked.txt"
  let cfgFile = "testsuite/annotations-shrinked/shrink.yaml"
  expected <- readFile shrinkedTxtPath
  shrinkingCfg <- BL.readFile cfgFile >>=
    mkShrinkingNodeConfig (const (Right . T.unpack)) (Right . T.unpack)
  xmlString :: String <- readFile xmlFile
  offsetMapping <- parsecOffsetMapping indexed xmlFile xmlString
  xml <- runXmlParser offsetMapping xmlFile xmlString
  (offsets, _txt) <- runWriterT (shrinkedText tell shrinkingCfg xml xmlString)

  mapM_ (uncurry (validateSingleMappedChar xmlString offsets)) $ zip [indexed ..] expected

    where
      validateSingleMappedChar :: HasCallStack => String -> OffsetMapping -> Int -> Char -> IO ()
      validateSingleMappedChar xmlString offsets pos c
        | c == 'd' = return () -- we drop 'd' characters in order to test with charref.xml
        | c == '\252' = return () -- we drop '&uuml;' characters in order to test with advanced.xml
        | isSpace c = return ()
        | isAscii c && isPunctuation c = validateChar xmlString offsets pos c
        | isLetter c = validateChar xmlString offsets pos c
        | otherwise = return ()
      validateChar :: HasCallStack => String -> OffsetMapping -> Int -> Char -> IO ()
      validateChar xmlString offsets pos c = do
        assertEqual c $ head $ drop (offsets !! pos) xmlString


-- * Run the tests on testsuite/annotations-shrinked/BASE.shrinked.txt

test_elementShrinkedTXT = do
  validateShrinkedTXT "element"

test_elementOffsetMapping = do
  validateOffsetMapping "element"

test_elementMappedChars = do
  validateMappedCharacters "element"


test_charrefShrinkedTXT = do
  validateShrinkedTXT "charref"

test_charrefOffsetMapping = do
  validateOffsetMapping "charref"

test_charrefMappedChars = do
  validateMappedCharacters "charref"


test_advancedShrinkedTXT = do
  validateShrinkedTXT "advanced"

test_advancedOffsetMapping = do
  validateOffsetMapping "advanced"

test_advancedMappedChars = do
  validateMappedCharacters "advanced"


test_commentShrinkedTXT = do
  validateShrinkedTXT "comment"

test_commentOffsetMapping = do
  validateOffsetMapping "comment"

test_commentMappedChars = do
  validateMappedCharacters "comment"


test_cdataShrinkedTXT = do
  validateShrinkedTXT "cdata"

test_cdataOffsetMapping = do
  validateOffsetMapping "cdata"

test_cdataMappedChars = do
  validateMappedCharacters "cdata"


test_emptyShrinkedTXT = do
  validateShrinkedTXT "empty"

test_emptyOffsetMapping = do
  validateOffsetMapping "empty"

test_emptyMappedChars = do
  validateMappedCharacters "empty"


test_piShrinkedTXT = do
  validateShrinkedTXT "pi"

test_piOffsetMapping = do
  validateOffsetMapping "pi"

test_piMappedChars = do
  validateMappedCharacters "pi"
