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


-- * Tests


test_positionsSimple = do
  assertEqual (Right True) =<< validatePositionsForFile "testsuite/simple.xml"

test_positionsSimple2 = do
  assertEqual (Right True) =<< validatePositionsForFile "testsuite/simple2.xml"

test_positionsSimple3 = do
  assertEqual (Right True) =<< validatePositionsForFile "testsuite/simple3.xml"

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
