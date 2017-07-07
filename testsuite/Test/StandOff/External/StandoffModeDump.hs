{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.External.StandoffModeDump (htf_thisModulesTests) where

import Test.Framework

import StandOff.AnnotationTypeDefs
import StandOff.External.StandoffModeDump

import Test.StandOff.TestSetup

import Data.UUID (UUID, toString, fromString)
import Text.Parsec (parse)
import Data.Maybe

test_uuidOk = do
  assertEqual sample $ either ((err++). show) toString $ parse uuid "(test_uuidParser)" quoted
  where
    sample = "f908b277-1841-46ad-94db-d16e4a94d0e3"
    quoted = "\"" ++ sample ++ "\""
    err = "Failed: "

test_uuidLeft = do
  assertLeft $ parse uuid "(test_uuidParser)" quoted
  where
    sample = "t908b277-1841-46ad-94db-d16e4a94d0e3"
    quoted = "\"" ++ sample ++ "\""

test_uuidFailure = do
  assertThrowsSome $ parse uuid "(test_uuidParser)" quoted
  where
    sample = "-908b277-1841-46ad-94db-d16e4a94d0e3"
    quoted = "\"" ++ sample ++ "\""

test_markupRangeWithoutRangeId = do
  assertRight $ parse markupRange "(test_markupRange)" sample
  case (parse markupRange "(test_markupRange)" sample) of
    Left err -> print err
    Right range -> do
      { assertEqual eid $ toString $ rangeElementId range
      ; assertEqual typ $ rangeType range
      ; assertEqual 19456 $ rangeStartOffset range -- emacs value corrected
      ; assertEqual 19603 $ rangeEndOffset range
      }
  where
    sample = "(\"" ++ eid ++ "\" \"" ++ typ ++ "\" 19457 19605 \"Text\" (22433 61483 931003 148000) \"unknown\")"
    eid = "98b99bbd-2b6d-4027-9f07-c141e73cbcb6"
    typ = "http://arb.fernuni-hagen.de/owl/beispiel#Konzept"
    
test_markupRange = do
  assertRight $ parse markupRange "(test_markupRange)" sample
  case (parse markupRange "(test_markupRange)" sample) of
    Left err -> print err
    Right range -> do
      { assertEqual eid $ toString $ rangeElementId range
      ; assertEqual rid $ fromMaybe "" $ fmap toString $ rangeRangeId range
      ; assertEqual typ $ rangeType range
      ; assertEqual 19456 $ rangeStartOffset range -- emacs value corrected
      ; assertEqual 19603 $ rangeEndOffset range
      }
  where
    sample = "(\"" ++ eid ++ "\" \"" ++ rid ++ "\" \"" ++ typ ++ "\" 19457 19605 \"Text\" (22433 61483 931003 148000) \"unknown\")"
    eid = "98b99bbd-2b6d-4027-9f07-c141e73cbcb6"
    rid = "f908b277-1841-46ad-94db-d16e4a94d0e3"
    typ = "http://arb.fernuni-hagen.de/owl/beispiel#Konzept"

test_markupRangeOldVersion = do
  assertRight $ parse markupRange "(test_markupRange)" sample
  where
    sample = "(\"a58efe8c-0da9-4b2e-a6f5-22e67d3f2e2a\" \"beispiel\" 19644 19687 \"den Liebhaber hinter der dicken<lb/>Tapete\")"


test_relation = do
  assertRight $ parse relation "(test_relation)" sample
  case (parse relation "(test_relation)" sample) of
    Left err -> print err
    Right rel -> do
      { assertEqual rid $ fromMaybe "" $ fmap toString $ relationRelationId rel
      ; assertEqual subj $ toString $ relationSubject rel
      ; assertEqual pred $ relationPredicate rel
      ; assertEqual obj $ toString $ relationObject rel
      }
  where
    sample = "(\"" ++ rid ++ "\" \"" ++ subj ++ "\" \"" ++ pred ++ "\" \"" ++ obj ++ "\" (22433 62212 145385 526000) \"unknown\")"
    rid = "3385423d-6aec-47cb-b225-7c0ae851662c"
    subj = "b3631d5d-17a9-4e3f-a267-1f7c8ad5175e"
    pred = "http://arb.fernuni-hagen.de/owl/beispiel#reihtAn"
    obj = "69f0a2cb-3a16-4e95-a985-8459889366cc"

test_relationWithoutRelationId = do
  assertRight $ parse relation "(test_relation)" sample
  case (parse relation "(test_relation)" sample) of
    Left err -> print err
    Right rel -> do
      { assertEqual subj $ toString $ relationSubject rel
      ; assertEqual pred $ relationPredicate rel
      ; assertEqual obj $ toString $ relationObject rel
      }
  where
    sample = "(\"" ++ subj ++ "\" \"" ++ pred ++ "\" \"" ++ obj ++ "\" (22433 62212 145385 526000) \"unknown\")"
    subj = "b3631d5d-17a9-4e3f-a267-1f7c8ad5175e"
    pred = "http://arb.fernuni-hagen.de/owl/beispiel#reihtAn"
    obj = "69f0a2cb-3a16-4e95-a985-8459889366cc"
