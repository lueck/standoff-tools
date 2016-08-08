module TestSetup where

import StandOff.Data.Annotation as A
import StandOff.Data.XML
import StandOff.XML.LineOffsets as L

pos :: Int -> L.Position
pos p = L.Position {L.offset=p, L.line=1, L.column=1}

elm :: String -> Int -> Int -> [XML] -> XML
elm n s e c = (Element { name = n
                       , attributes = []
                       , startOpenTag = pos s
                       , endOpenTag = pos (s + openTagLength - 1)
                       , startCloseTag = pos (e - openTagLength)
                       , endCloseTag = pos e
                       , content = c })
  where openTagLength = 2 + (length n)

mRng :: String -> String -> String -> Int -> Int -> A.Annotation
mRng rid mid typ s e = (A.MarkupRange { A.rangeId = rid
                                      , A.elementId = mid
                                      , A.markupType = typ
                                      , A.startOffset = s
                                      , A.endOffset = e
                                      , A.text = "" })
