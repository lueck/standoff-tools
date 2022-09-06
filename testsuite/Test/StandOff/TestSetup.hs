module Test.StandOff.TestSetup where

import qualified Data.Tree.NTree.TypeDefs as NT
import Data.Tree.Class hiding (getNode)
import Data.Map as Map hiding (drop)
import Data.UUID.Types (UUID, fromString, toString)
import Data.Maybe

import StandOff.AnnotationTypeDefs as A
import StandOff.DomTypeDefs as X
import StandOff.LineOffsets as L
import StandOff.Tag
import StandOff.External.GenericCsv

pos :: Int -> Int
pos p = p -- L.Position {L.pos_offset=p, L.pos_line=1, L.pos_column=1}

elm :: String -> Int -> Int -> XMLTrees Int String String -> XMLTree Int String String
elm n s e c = NT.NTree (Element { name = n
                       , X.attributes = []
                       , startOpenTag = pos s
                       , endOpenTag = pos (s + openTagLength - 1)
                       , startCloseTag = pos (e - openTagLength)
                       , endCloseTag = pos e}) c
  where openTagLength = 2 + (length n)

-- For easy setup of (uu)ids for unit tests:
testUUID = "00000000-0000-0000-0000-000000000000"
mkTestUUID :: String -> UUID
mkTestUUID start =
  case fromString strUuid of
    Nothing -> error ("Invalid UUID: " ++ strUuid)
    (Just u) -> u
  where strUuid = start ++ (drop (length start) testUUID)

mRng :: String -> String -> String -> Int -> Int -> A.Annotation
mRng rid mid typ s e = (A.MarkupRange { A.rangeId = Just $ mkTestUUID rid
                                      , A.elementId = mkTestUUID mid
                                      , A.markupType = typ
                                      , A.startOffset = s
                                      , A.endOffset = e
                                      , A.text = Just ""
                                      , A.attributes = Map.empty })

getNthNode :: [Int] -> XMLTrees p n s -> XMLTree p n s
getNthNode [] ts = head ts
getNthNode (n:[]) ts = ts !! n
getNthNode (n:ns) ts = getNthNode ns (getChildren $ ts !! n)


-- | Simple tag serializer for testing purpose. The tag name is
-- related to the constant tag name defined in
-- testsuite/annotations/Makefile!
aTagSerializer :: TagType -> GenericCsvMarkup -> String
aTagSerializer (Open) annot = "<ANNOT" ++ caseAttributeSerializer annot ++ ">"
aTagSerializer (Close) _ = "</ANNOT>"
aTagSerializer (Empty) annot = "<ANNOT" ++ caseAttributeSerializer annot ++ "/>"

caseAttributeSerializer :: GenericCsvMarkup -> String
caseAttributeSerializer markup =
  fromMaybe "" $ fmap (\c -> " case=\"" ++ c ++ "\"") $ Map.lookup "case" $ ncsv_features markup
