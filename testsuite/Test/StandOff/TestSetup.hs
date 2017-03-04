module Test.StandOff.TestSetup where

import Data.Map as Map
import Data.UUID (UUID, fromString, toString)

import StandOff.AnnotationTypeDefs as A
import StandOff.DomTypeDefs as X
import StandOff.LineOffsets as L

pos :: Int -> L.Position
pos p = L.Position {L.offset=p, L.line=1, L.column=1}

elm :: String -> Int -> Int -> [XML] -> XML
elm n s e c = (Element { name = n
                       , X.attributes = []
                       , startOpenTag = pos s
                       , endOpenTag = pos (s + openTagLength - 1)
                       , startCloseTag = pos (e - openTagLength)
                       , endCloseTag = pos e
                       , content = c })
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
                                      , A.text = ""
                                      , A.attributes = Map.empty })
