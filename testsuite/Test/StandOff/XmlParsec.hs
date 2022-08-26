{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.XmlParsec (htf_thisModulesTests) where

import Test.Framework

import Data.Traversable
import Data.Foldable
import Control.Monad.State
import Data.Tree.Class
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import StandOff.DomTypeDefs hiding (start, end, getNode)
import StandOff.XmlParsec
import StandOff.StringLike (StringLike)
import StandOff.OffsetMapping
import qualified StandOff.StringLike as SL
import StandOff.XTraverse
import StandOff.TextRange


-- * Helpers

indexed = 0

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
  -> XmlNode Int n s
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

charAt :: StringLike s => Int -> s -> Char
charAt pos = SL.head . SL.drop pos


validatePositionsForFile :: FilePath -> IO (Either String Bool)
validatePositionsForFile fPath = do
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  r <- xtraverseWithState (const (return ())) (validatePositions c) (validatePositions c) xml (Right True)
  return r


getNthNode :: [Int] -> XMLTrees p n s -> XMLTree p n s
getNthNode [] ts = head ts
getNthNode (n:[]) ts = ts !! n
getNthNode (n:ns) ts = getNthNode ns (getChildren $ ts !! n)


myMapTuple :: (a -> b) -> (a, a) -> (b, b)
myMapTuple f (a1, a2) = (f a1, f a2)


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
  unitTestPending "Is there a trailing white space text node?"
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual [ElementNode, TextNodeType] $ map (nodeType . getNode) xml


-- | Assert ground truth manually.
test_zeroIndexed = do
  let fPath = "testsuite/simple.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
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
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
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
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
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
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
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
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  let c1 = getNode $ getNthNode [2] xml
  assertEqual (0x27, 0x34) $ spans c1
  assertEqual CommentNode $ nodeType c1
  let c2 = getNode $ getNthNode [4, 3, 3] xml
  assertEqual (0x93, 0x9f) $ spans c2
  assertEqual CommentNode $ nodeType c2
  let c3 = getNode $ getNthNode [6] xml
  assertEqual CommentNode $ nodeType c3
  assertEqual (0x350, 0x35a) $ spans c3
  let c4 = getNode $ getNthNode [8] xml
  assertEqual CommentNode $ nodeType c4
  assertEqual (0x35c, 0x368) $ spans c4

test_cdata = do
  let fPath = "testsuite/cdata.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  let c1 = getNode $ getNthNode [0, 3] xml
  assertEqual CDataNode $ nodeType c1


test_charRefDec = do
  let fPath = "testsuite/charref.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  let n = getNode $ getNthNode [0, 1] xml
  assertEqual CharRefNode $ nodeType n
  assertEqual 0x64 $ char n

test_charRefHex = do
  let fPath = "testsuite/charref.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  let n = getNode $ getNthNode [0, 3] xml
  assertEqual CharRefNode $ nodeType n
  assertEqual 0x64 $ char n

test_advancedNodes = do
  --unitTestPending "Parsec updates the position on \\t characters in a special way."
  let fPath = "testsuite/advanced.xml"
  c <- readFile fPath
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual XMLDeclarationNode $ nodeType $ getNode $ getNthNode [0] xml
  assertEqual (0, 0x25) $ spans $ getNode $ getNthNode [0] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [1] xml
  assertEqual (0x26, 0x26) $ spans $ getNode $ getNthNode [1] xml
  assertEqual ElementNode $ nodeType $ getNode $ getNthNode [2] xml
  assertEqual (0x27, 0xec) $ spans $ getNode $ getNthNode [2] xml
  assertEqual (0x27, 0x30)  $ openTagRange $ getNode $ getNthNode [2] xml
  assertEqual (0xe2, 0xec)  $ closeTagRange $ getNode $ getNthNode [2] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,0] xml
  assertEqual (0x31, 0x33) $ spans $ getNode $ getNthNode [2,0] xml
  assertEqual EmptyElementNode $ nodeType $ getNode $ getNthNode [2,1] xml
  assertEqual (0x34, 0x3a) $ spans $ getNode $ getNthNode [2,1] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,2] xml
  assertEqual (0x3b, 0x3d) $ spans $ getNode $ getNthNode [2,2] xml
  assertEqual ElementNode $ nodeType $ getNode $ getNthNode [2,3] xml
  assertEqual (0x3e, 0xe0) $ spans $ getNode $ getNthNode [2,3] xml
  assertEqual (0x3e, 0x43)  $ openTagRange $ getNode $ getNthNode [2,3] xml
  assertEqual (0xda, 0xe0)  $ closeTagRange $ getNode $ getNthNode [2,3] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,0] xml
  assertEqual EmptyElementNode $ nodeType $ getNode $ getNthNode [2,3,1] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,2] xml
  assertEqual CommentNode $ nodeType $ getNode $ getNthNode [2,3,3] xml
  assertEqual (0x63, 0x8f) $ spans $ getNode $ getNthNode [2,3,3] xml -- <<<
  assertEqual " missing? ---\n    <h1\n\t>First Head</h1" $ text $ getNode $ getNthNode [2,3,3] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,4] xml
  assertEqual (0x90, 0x94) $ spans $ getNode $ getNthNode [2,3,4] xml -- <<<
  assertEqual ElementNode $ nodeType $ getNode $ getNthNode [2,3,5] xml
  assertEqual (0x95, 0xd6) $ spans $ getNode $ getNthNode [2,3,5] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,5,0] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,6] xml
  -- assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,7] xml -- does not exist
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,4] xml

-- | We to work around the problem of parsec and tabs, we replace
-- every tab character on the input stream with a space.
test_advancedNodesWithoutTabs = do
  let fPath = "testsuite/advanced.xml"
  c' <- BS.readFile fPath
  let withtabs =  T.decodeUtf8 c'
  let c = T.map replaceTab withtabs
  offsetMapping <- parsecOffsetMapping indexed (show fPath) c
  xml <- runXmlParser offsetMapping (show fPath) c
  assertEqual XMLDeclarationNode $ nodeType $ getNode $ getNthNode [0] xml
  assertEqual (0, 0x25) $ spans $ getNode $ getNthNode [0] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [1] xml
  assertEqual (0x26, 0x26) $ spans $ getNode $ getNthNode [1] xml
  assertEqual ElementNode $ nodeType $ getNode $ getNthNode [2] xml
  assertEqual (0x27, 0xec) $ spans $ getNode $ getNthNode [2] xml
  assertEqual (0x27, 0x30)  $ openTagRange $ getNode $ getNthNode [2] xml
  assertEqual (0xe2, 0xec)  $ closeTagRange $ getNode $ getNthNode [2] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,0] xml
  assertEqual (0x31, 0x33) $ spans $ getNode $ getNthNode [2,0] xml
  assertEqual EmptyElementNode $ nodeType $ getNode $ getNthNode [2,1] xml
  assertEqual (0x34, 0x3a) $ spans $ getNode $ getNthNode [2,1] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,2] xml
  assertEqual (0x3b, 0x3d) $ spans $ getNode $ getNthNode [2,2] xml
  assertEqual ElementNode $ nodeType $ getNode $ getNthNode [2,3] xml
  assertEqual (0x3e, 0xe0) $ spans $ getNode $ getNthNode [2,3] xml
  assertEqual (0x3e, 0x43)  $ openTagRange $ getNode $ getNthNode [2,3] xml
  assertEqual (0xda, 0xe0)  $ closeTagRange $ getNode $ getNthNode [2,3] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,0] xml
  assertEqual EmptyElementNode $ nodeType $ getNode $ getNthNode [2,3,1] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,2] xml
  assertEqual CommentNode $ nodeType $ getNode $ getNthNode [2,3,3] xml
  assertEqual (0x63, 0x8f) $ spans $ getNode $ getNthNode [2,3,3] xml -- <<<
  -- \t was replaced with space
  assertEqual " missing? ---\n    <h1\n >First Head</h1" $ text $ getNode $ getNthNode [2,3,3] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,4] xml
  assertEqual (0x90, 0x94) $ spans $ getNode $ getNthNode [2,3,4] xml -- <<<
  assertEqual ElementNode $ nodeType $ getNode $ getNthNode [2,3,5] xml
  assertEqual (0x95, 0xd6) $ spans $ getNode $ getNthNode [2,3,5] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,5,0] xml
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,6] xml
  -- assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,3,7] xml -- does not exist
  assertEqual TextNodeType $ nodeType $ getNode $ getNthNode [2,4] xml
  where
    replaceTab :: Char -> Char
    replaceTab '\t' = ' '
    replaceTab c = c
