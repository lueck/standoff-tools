{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.XmlParsec (htf_thisModulesTests) where

import Test.Framework

import Data.Traversable
import Data.Foldable
import Control.Monad.State
import Data.Tree.Class

import StandOff.DomTypeDefs hiding (start, end, getNode)
import StandOff.XmlParsec
import StandOff.StringLike (StringLike)
import qualified StandOff.LineOffsets as LOFF
import qualified StandOff.StringLike as SL
import StandOff.XTraverse
import StandOff.TextRange
import StandOff.LineOffsets (Position(..))


-- * Helpers

-- traverseWithState
--   :: (Monad m, StringLike s) =>
--      (XmlNode n s -> a -> (b, a))
--   -> a
--   -> XMLTrees n s
--   -> m a
-- traverseWithState f initial xml = execStateT (traverse (\r -> state (f (getNode r))) xml) initial
-- -- no deep traversal!


validatePositions
  :: (StringLike s, Show n, Eq s) =>
     s
  -> XmlNode n s
  -> Either String Bool
  -> ((), Either String Bool)
validatePositions doc (Element n _ so eo sc ec) (Right st)
  | '<' /= charAt so doc = ((), Left $ show n ++ " No '<' at opening tag position, " ++ show so)
  | '>' /= charAt eo doc = ((), Left $ show n ++ " No '>' at opening tag position, " ++ show eo)
  | '<' /= charAt sc doc = ((), Left $ show n ++ " No '<' at closing tag position, " ++ show sc)
  | '>' /= charAt ec doc = ((), Left $ show n ++ " No '>' at closing tag position, " ++ show ec)
  | otherwise = ((), Right st)
validatePositions doc (EmptyElement n _ so eo) (Right st)
  | '<' /= charAt so doc = ((), Left $ show n ++ " No '<' at empty tag position, " ++ show so)
  | '>' /= charAt eo doc = ((), Left $ show n ++ " No '>' at empty tag position, " ++ show eo)
  | otherwise = ((), Right st)
validatePositions doc (XMLDeclaration _ so eo) (Right st)
  | '<' /= charAt so doc = ((), Left $ "No '<' at declaration position, " ++ show so)
  | '>' /= charAt eo doc = ((), Left $ "No '>' at declaration position, " ++ show eo)
  | otherwise = ((), Right st)
validatePositions doc (ProcessingInstruction n _ so eo) (Right st)
  | '<' /= charAt so doc = ((), Left $ show n ++ " No '<' at PI position, " ++ show so)
  | '>' /= charAt eo doc = ((), Left $ show n ++ " No '>' at PI position, " ++ show eo)
  | otherwise = ((), Right st)
validatePositions doc (CharRef n so eo) (Right st)
  | '&' /= charAt so doc = ((), Left $ show n ++ " char ref: No '&' at char ref position, " ++ show so)
  | ';' /= charAt eo doc = ((), Left $ show n ++ " char ref: No ';' at char ref position, " ++ show eo)
  | otherwise = ((), Right st)
validatePositions doc (EntityRef n so eo) (Right st)
  | '&' /= charAt so doc = ((), Left $ show n ++ " No '&' at entity ref position, " ++ show so)
  | ';' /= charAt eo doc = ((), Left $ show n ++ " No ';' at entity position, " ++ show eo)
  | otherwise = ((), Right st)
validatePositions doc (CData _ so eo) (Right st)
  | '<' /= charAt so doc = ((), Left $ "No '<' at CData position, " ++ show so)
  | '>' /= charAt eo doc = ((), Left $ "No '>' at CData position, " ++ show eo)
  | otherwise = ((), Right st)
-- TODO: other node types
validatePositions _ _ st = ((), st)

charAt :: StringLike s => LOFF.Position -> s -> Char
charAt pos = SL.head . SL.drop (LOFF.posOffset pos)


validatePositionsForFile :: FilePath -> IO (Either String Bool)
validatePositionsForFile fPath = do
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  r <- xtraverseWithState (const (return ())) (validatePositions c) (validatePositions c) xml (Right True)
  return r


getNthNode :: [Int] -> XMLTrees n s -> XMLTree n s
getNthNode [] ts = head ts
getNthNode (n:[]) ts = ts !! n
getNthNode (n:ns) ts = getNthNode ns (getChildren $ ts !! n)


-- * Tests


test_positionsSimple = do
  assertEqual (Right True) =<< validatePositionsForFile "testsuite/simple.xml"

test_positionsAdvanced = do
  assertEqual (Right True) =<< validatePositionsForFile "testsuite/advanced.xml"

test_positionsPI = do
  assertEqual (Right True) =<< validatePositionsForFile "testsuite/pi.xml"

test_positionsCharRef = do
  assertEqual (Right True) =<< validatePositionsForFile "testsuite/charref.xml"

test_positionsComment = do
  assertEqual (Right True) =<< validatePositionsForFile "testsuite/comment.xml"

test_positionsCData = do
  assertEqual (Right True) =<< validatePositionsForFile "testsuite/cdata.xml"


test_forrestSimple = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  assertEqual [ElementNode, TextNodeType] $ map (nodeType . getNode) xml


-- | Assert ground truth manually.
test_zeroIndexed = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  -- /document
  let root = getNode $ head xml
  assertEqual 0 $ start root
  assertEqual 0xad8 $ end root
  -- /document[1]/head[1]
  let headd = getNode $ (!! 1) $ getChildren $ head xml
  assertEqual 0x15 $ start headd
  assertEqual 0x23 $ end headd

-- | for text nodes we need to test manually
test_textNodePositions = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  -- /document[1]/body[1]/header[1]/text()
  let headerTxt = getNode $ head $ getChildren $ (!! 1) $ getChildren $ (!! 3) $ getChildren $ head xml
  assertEqual TextNodeType $ nodeType headerTxt
  assertEqual "First Heading" $ text headerTxt
  assertEqual 0x46 $ start headerTxt
  assertEqual 0x52 $ end headerTxt

-- | for text nodes we need to test manually. We have an extrac parser
-- for whitespace nodes for the prolog.
test_whitespaceNodePositions = do
  let fPath = "testsuite/pi.xml"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  -- /document[1]/body[1]/header[1]/text()
  let ws = getNode $ getNthNode [1] xml
  assertEqual TextNodeType $ nodeType ws
  assertEqual "\n" $ take 1 $ flip drop c $ start ws
  assertEqual "\n" $ take 1 $ flip drop c $ end ws
  assertEqual ">" $ take 1 $ flip drop c $ (\i -> i - 1) $ start ws
  assertEqual "<" $ take 1 $ flip drop c $ (\i -> i + 1) $ end ws

test_pi = do
  let fPath = "testsuite/pi.xml"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  let decl = getNode $ getNthNode [0] xml
  assertEqual XMLDeclarationNode $ nodeType decl
  let pi1 = getNode $ getNthNode [2] xml
  assertEqual ProcessingInstructionNode $ nodeType pi1
  let pi2 = getNode $ getNthNode [2] xml
  assertEqual ProcessingInstructionNode $ nodeType pi2
  let pi2 = getNode $ getNthNode [6, 3] xml
  assertEqual ProcessingInstructionNode $ nodeType pi2

test_comment = do
  let fPath = "testsuite/comment.xml"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  let c1 = getNode $ getNthNode [2] xml
  assertEqual CommentNode $ nodeType c1
  let c2 = getNode $ getNthNode [4, 3, 3] xml
  assertEqual CommentNode $ nodeType c2
  let c3 = getNode $ getNthNode [6] xml
  assertEqual CommentNode $ nodeType c3
  let c4 = getNode $ getNthNode [8] xml
  assertEqual CommentNode $ nodeType c4

test_cdata = do
  let fPath = "testsuite/cdata.xml"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  let c1 = getNode $ getNthNode [0, 3] xml
  assertEqual CDataNode $ nodeType c1


test_charRefDec = do
  let fPath = "testsuite/charref.xml"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  let n = getNode $ getNthNode [0, 1] xml
  assertEqual CharRefNode $ nodeType n
  assertEqual 0x64 $ char n

test_charRefHex = do
  let fPath = "testsuite/charref.xml"
  c <- readFile fPath
  lOffsets <- LOFF.runLineOffsetParser (show fPath) c
  xml <- runXmlParser lOffsets (show fPath) c
  let n = getNode $ getNthNode [0, 3] xml
  assertEqual CharRefNode $ nodeType n
  assertEqual 0x64 $ char n
