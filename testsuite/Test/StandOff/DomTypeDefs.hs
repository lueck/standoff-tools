{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TupleSections #-}
module Test.StandOff.DomTypeDefs (htf_thisModulesTests) where

import Test.Framework
import Data.Tree.Class (getNode)
import System.IO
import Data.Text.Encoding (decodeUtf8)
import GHC.Stack (HasCallStack)
import qualified Data.Map as Map
import Data.Maybe
import Numeric
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Bin

import StandOff.DomTypeDefs (XmlNode, XMLTrees)
import StandOff.TextRange
import StandOff.XmlParsec
import StandOff.SourcePosMapping
import StandOff.Splitting
import StandOff.MarkupTree hiding (getNode)
import StandOff.External.GenericCsv
import StandOff.Internalize
import StandOff.ShrinkedText

import Test.StandOff.TestSetup

test_elementImplementsTextRange = do
  assertEqual 100 (start d)
  assertEqual 200 (end d)
  assertEqual (100, 200) (spans d)
  assertEqual ((99, 105), (194, 201)) (splitPoints d)
  assertEqual 100 (len d)
  assertThrowsSome (split FstSplit d)
  assertThrowsSome (leftSplit FstSplit d (getNode $ elm "x" 120 350 []))
  assertThrowsSome (rightSplit FstSplit d (getNode $ elm "x" 12 150 []))
  assertEqual True (d <<>> (getNode $ elm "span" 120 150 []))
  assertEqual True (d <<>> (getNode $ elm "span" 100 150 []))
  assertEqual True (d <<>> (getNode $ elm "span" 120 200 []))
  assertEqual False (d <<>> (getNode $ elm "span" 120 210 []))
  assertEqual True (d `before` (getNode $ elm "div" 210 300 []))
  assertEqual False (d `before` (getNode $ elm "div" 200 300 []))
  assertEqual True (flip startRightForbidden d (getNode $ elm "div" 200 300 []))
  assertEqual False (d `before` (getNode $ elm "div" 10 30 []))
  assertEqual True (d `behind` (getNode $ elm "div" 10 30 []))
  assertEqual False (d `behind` (getNode $ elm "div" 10 100 []))
  assertEqual True (flip endLeftForbidden d (getNode $ elm "div" 10 100 []))
  assertEqual False (d `behind` (getNode $ elm "div" 210 300 []))
  assertEqual True (d `leftOverlaps` (getNode $ elm "div" 190 300 []))
  assertEqual False (d `leftOverlaps` (getNode $ elm "div" 90 130 []))
  assertEqual True (d `rightOverlaps` (getNode $ elm "div" 90 130 []))
  assertEqual False (d `rightOverlaps` (getNode $ elm "div" 210 300 []))
  where
    d :: XmlNode Int String String
    d = getNode $ elm "div" 100 200 ([]::XMLTrees Int String String)


mergeCase internal' s e = map spans $ merge internal' (mRng "a" "a" "a" s e)

mergeCases :: (MarkupTree t a, TextRange a) => [t a] -> [(Int, Int)] -> [(Int, Int)]
mergeCases internal' annots = map spans $ splitOverlapping internal' $ map (uncurry (mRng "a" "a" "a")) annots

indexed = 0

test_splitSimple = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual [(0x68, 0x72)] $ mergeCase xml 0x68 0x72
  assertEqual [(0x68, 0x72)] $ mergeCase xml 0x65 0x72

test_splitCharRefEndOnRef = do
  let fPath = "testsuite/charref.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual [(0x0a, 0x1f)] $ mergeCase xml 0x0a 0x1f
  assertEqual [(0x0a, 0x15)] $ mergeCase xml 0x0a 0x18
  assertEqual [(0x0a, 0x15)] $ mergeCase xml 0x0a 0x19
  assertEqual [(0x0a, 0x15)] $ mergeCase xml 0x0a 0x1a
  assertEqual [(0x0a, 0x1b)] $ mergeCase xml 0x0a 0x1b -- semicolon last char of charref, so contain it!
  assertEqual [(0x0a, 0x1c)] $ mergeCase xml 0x0a 0x1c

test_splitCharRefStartOnRef = do
  let fPath = "testsuite/charref.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual [(0x1c, 0x1f)] $ mergeCase xml 0x19 0x1f
  assertEqual [(0x1c, 0x1f)] $ mergeCase xml 0x1a 0x1f
  assertEqual [(0x1c, 0x1f)] $ mergeCase xml 0x1b 0x1f -- <<<
  assertEqual [(0x1c, 0x1f)] $ mergeCase xml 0x1c 0x1f
  assertEqual [(0x1d, 0x1f)] $ mergeCase xml 0x1d 0x1f

test_splitEntityRefEndOnRef = do
  let fPath = "testsuite/entityref.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual [(0x29, 0x2c)] $ mergeCase xml 0x29 0x2e
  assertEqual [(0x29, 0x2c)] $ mergeCase xml 0x29 0x2d
  assertEqual [(0x29, 0x2c)] $ mergeCase xml 0x29 0x2c
  assertEqual [(0x29, 0x2b)] $ mergeCase xml 0x29 0x2b
  -- assertEqual [(0x29, 0x2c)] $ mergeCase xml 0x29 0x1f

test_splitEntityRefStartOnRef = do
  let fPath = "testsuite/entityref.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual [(0x37, 0x3b)] $ mergeCase xml 0x33 0x3b
  assertEqual [(0x37, 0x3b)] $ mergeCase xml 0x34 0x3b
  assertEqual [(0x37, 0x3b)] $ mergeCase xml 0x35 0x3b
  assertEqual [(0x37, 0x3b)] $ mergeCase xml 0x36 0x3b
  assertEqual [(0x37, 0x3b)] $ mergeCase xml 0x37 0x3b
  assertEqual [(0x38, 0x3b)] $ mergeCase xml 0x38 0x3b


test_splitSimpleMerge = do
  let fPath = "testsuite/charref.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual [(0x0a, 0x1f)] $ mergeCase xml 0x0a 0x1f
  assertEqual [(0x0a, 0x1f)] $ mergeCases xml [(0x0a, 0x1f)]

test_splitSimpleSplitOverlapping = do
  let fPath = "testsuite/charref.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual [(0x0a, 0x1f)] $ map spans $ splitOverlapping xml [mRng "a" "a" "a" 0x0a 0x1f]

test_splitSimpleSplitOverlappingCSV = do
  let fPath = "testsuite/charref.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  anh <- openFile "testsuite/annotations/simple.end-left-forbidden.csv" ReadMode
  ans <- runCsvParser startEndMarkup decodeUtf8 anh
  assertEqual [(0x0a, 0x1f)] $ map spans $ splitOverlapping xml $ [head ans]

test_splitSimpleInternalizeCSV = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  anh <- openFile "testsuite/annotations/simple.end-left-forbidden.csv" ReadMode
  ans <- runCsvParser startEndMarkup decodeUtf8 anh
  assertEqual "<document id=\"i1\"><ANNOT>\n  </ANNOT><head id=\"i2\"/>\n  " $
    take 54 $ internalize c xml [head ans] aTagSerializer


-- * Validatations based on testsuite/annotations/*.csv

-- ** Helper functions

-- | Test helper for validating the tests from
-- testsuite/annotations/BASE.TEST-NAME.csv. Also see
-- testsuite/annotations/Makefile.
validateCsvCases
  :: HasCallStack =>
     String -- ^ the name of the XML base file (BASE)
  -> String -- ^ the name of the test (TEST-NAME)
  -> IO ()
validateCsvCases base testName = do
  let fPath = "testsuite/" ++ base ++ ".xml"
  let aPath = "testsuite/annotations/" ++ base ++ "." ++ testName
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  anh <- openFile (aPath  ++ ".csv") ReadMode
  ans <- runCsvParser startEndMarkup decodeUtf8 anh
  mapM_ (uncurry (validateInternalizedXML c xml aPath)) (zip [1..] ans)

-- | Helper function for validating a single CSV test case.
validateInternalizedXML
  :: (HasCallStack, MarkupTree t a, TextRange a) =>
     String
  -> [t a]
  -> String
  -> Int
  -> GenericCsvMarkup
  -> IO ()
validateInternalizedXML xmlString xmlDom aPath caseNum annot = do
  let internalizedPath = aPath ++ "." ++ (leftFillZero 2 caseNum) ++ ".internalized.xml"
  expected <- readFile internalizedPath
  assertEqual expected $ internalize xmlString xmlDom [annot] aTagSerializer
  where
    leftFillZero :: Int -> Int -> String
    leftFillZero l i = (replicate (l - length (show i)) '0') ++ show i

-- | A helper function to run test cases from
-- testsuite/annoations/BASE.TEST-NAME.csv on the merge function.
--
-- The expected value is of a test case is of type (Int, [(Position,
-- Position)]), where the Int is the case number (CASE-NUMBER).
validateMergeCasesFromCSV
  :: HasCallStack =>
     String -- ^ the name of the XML base file (BASE)
  -> String -- ^ the name of the test (TEST-NAME)
  -> IO ()
validateMergeCasesFromCSV base testName = do
  let fPath = "testsuite/" ++ base ++ ".xml"
  let aPath = "testsuite/annotations/" ++ base ++ "." ++ testName
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  anh <- openFile (aPath  ++ ".csv") ReadMode
  ans <- runCsvParser startEndMarkup decodeUtf8 anh
  mapM_ (uncurry (validateCsvMergeCase xml)) (zip [1..] ans)

-- | Helper function for validating a single CSV test case.
validateCsvMergeCase
  :: (HasCallStack, MarkupTree t a, TextRange a) =>
     [t a]
  -> Int
  -> GenericCsvMarkup
  -> IO ()
validateCsvMergeCase xmlDom caseNum annot = do
  assertEqual expected $ (caseNum,) $ map spans $ merge xmlDom annot
  where
    expected :: (Int, [(Int, Int)])
    expected
      | isJust $ maybeExpectedStart = (caseNum, [(fromMaybe (-1) maybeExpectedStart, annotEnd)])
      | isJust $ maybeExpectedEnd = (caseNum, [(annotStart, fromMaybe (-2) maybeExpectedEnd)])
      | otherwise = (caseNum, [])
    maybeExpectedStart = join $ fmap myReadHex $ Map.lookup "expected-start" features
    maybeExpectedEnd = join $ fmap myReadHex $ Map.lookup "expected-end" features
    features = ncsv_features annot
    myReadHex s
      | length s == 0 = Nothing
      | take 2 s == "0x" = Just $ fst $ head $ readHex $ drop 2 s
      | otherwise = Just $ fst $ head $ readDec s
    annotStart = fst $ spans annot
    annotEnd = snd $ spans annot


-- | Test helper for validating the tests from
-- testsuite/annotations-shrinked/BASE.TEST-NAME.csv. Also see
-- testsuite/annotations-shrinked/Makefile.
validateShrinkedInternalizationCases
  :: HasCallStack =>
     String -- ^ the name of the XML base file (BASE)
  -> String -- ^ the name of the test (TEST-NAME)
  -> IO ()
validateShrinkedInternalizationCases base testName = do
  let fPath = "testsuite/" ++ base ++ ".xml"
  let aPath = "testsuite/annotations-shrinked/" ++ base ++ "." ++ testName
  let offsetsFile = "testsuite/annotations-shrinked/" ++ base ++ ".offsets.dat"
  offsetMapping <- BL.readFile offsetsFile >>= return . Bin.decode
  c <- readFile fPath
  sourcePosMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser sourcePosMapping (show fPath) c
  anh <- openFile (aPath  ++ ".csv") ReadMode
  ans <- runCsvParser startEndMarkup decodeUtf8 anh
  ans' <- either fail return $ traverse (inflate offsetMapping) ans
  mapM_ (uncurry (validateInternalizedXML c xml aPath)) (zip [1..] ans')



-- ** Elements

test_internalizeCSVElementEndLeftForbidden = validateCsvCases "element" "end-left-forbidden"

test_mergeCSVElementEndLeftForbidden = validateMergeCasesFromCSV "element" "end-left-forbidden"


test_internalizeCSVElementStartLeftForbidden = validateCsvCases "element" "start-left-forbidden"

test_mergeCSVElementStartLeftForbidden = validateMergeCasesFromCSV "element" "start-left-forbidden"


test_internalizeCSVElementEndRightForbidden = validateCsvCases "element" "end-right-forbidden"

test_mergeCSVElementEndRightForbidden = validateMergeCasesFromCSV "element" "end-right-forbidden"


test_internalizeCSVElementStartRightForbidden = do
  validateCsvCases "element" "start-right-forbidden"

test_mergeCSVElementStartRightForbidden = do
  validateMergeCasesFromCSV "element" "start-right-forbidden"


test_internalizeShrinkedElementMove16 = do
  validateShrinkedInternalizationCases "element" "move16"


-- ** Character references

test_internalizeCSVCharRefEndForbidden = do
  validateCsvCases "charref" "end-forbidden"

test_mergeCSVCharRefEndForbidden = do
  validateMergeCasesFromCSV "charref" "end-forbidden"


test_internalizeCSVCharRefStartForbidden = do
  validateCsvCases "charref" "start-forbidden"

test_mergeCSVCharRefStartForbidden = do
  validateMergeCasesFromCSV "charref" "start-forbidden"


test_internalizeCSVCharRefMove7 = do
  -- unitTestPending "We cannot yust include the charref. See case 8 and 9."
  validateCsvCases "charref" "move7"


test_internalizeShrinkedCharRefMove16 = do
  unitTestPending "charref in internalizing from shrinked: should be included if annotation starts this character. See cases 3."
  validateShrinkedInternalizationCases "charref" "move16"



-- ** entity references

test_internalizeCSVEnityRefEndForbidden = do
  validateCsvCases "entityref" "end-forbidden"

test_mergeCSVEnityRefEndForbidden = do
  validateMergeCasesFromCSV "entityref" "end-forbidden"

test_internalizeCSVEntityRefStartForbidden = do
  validateCsvCases "entityref" "start-forbidden"

test_mergeCSVEntityRefStartForbidden = do
  validateMergeCasesFromCSV "entityref" "start-forbidden"



-- ** comment

test_internalizeShrinkedCommentMove16 = do
  -- Seems like the space in cases 3 and 5 is broken. But it is
  -- correct, because the annotation spans 3 characters outside of the
  -- comment.
  validateShrinkedInternalizationCases "comment" "move3"

