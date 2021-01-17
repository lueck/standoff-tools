{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.StandOff.InternalizerRW (htf_thisModulesTests) where

-- * Test valid xml output of internalize using real world data.

import Test.Framework
import Data.Either
import Text.Parsec
import Text.XML.HXT.Parser.XmlCharParser
import Text.XML.HXT.Parser.XmlParsec

import StandOff.ResolveOverlapping
import StandOff.Internalize
import StandOff.TextRange
import StandOff.External.StandoffModeDump
import StandOff.XmlParsec
import StandOff.LineOffsets
import StandOff.TagSerializer
import StandOff.AttributeSerializer
import StandOff.TagTypeDefs
import StandOff.AnnotationTypeDefs
import StandOff.DomTypeDefs

import Test.StandOff.TestSetup

sampleELisp = "doc/examples/XXX.TEI-P5.dump.el"
sampleXml = "doc/examples/XXX.TEI-P5.xml"

-- | Internalize using the fast internalizer
validateInternalized sampleXml = do
  elispContents <- readFile sampleELisp
  annotations <- runELispDumpParser sampleELisp elispContents
  xmlContents <- readFile sampleXml
  offsets <- runLineOffsetParser sampleXml xmlContents
  xml <- runXmlParser offsets sampleXml xmlContents
  let
    tagSerializer = serializeSpanTag
                    (serializeAttributes (Just "rid") (Just "eid") (Just "type") LocalName)
                    "span"
    internalized = (internalize
                    xmlContents
                    (filter isElementP xml)
                    (makeAttributiveRanges annotations)
                    tagSerializer)
  -- assert that there is valid xml. We use hxt's parser for this.
  return $ runParser document' (withNormNewline ()) ("("++sampleXml++")") internalized
  where
    sampleELisp = sampleXml ++ ".dump.el"

test_herder = do
  sample <- validateInternalized "doc/examples/herder_plastik_1778.TEI-P5.xml"
  assertBool(isRight sample)

test_rosenkranz = do
  sample <- validateInternalized "doc/examples/rosenkranz_aesthetik_1853.TEI-P5.xml"
  assertBool(isRight sample)

-- | Internalize and assert valid xml using the old and slow internalizer
-- implementation of internalize.
validateInternalized' sampleXml = do
  elispContents <- readFile sampleELisp
  annotations <- runELispDumpParser sampleELisp elispContents
  xmlContents <- readFile sampleXml
  offsets <- runLineOffsetParser sampleXml xmlContents
  xml <- runXmlParser offsets sampleXml xmlContents
  let
    tagSerializer = serializeSpanTag
                    (serializeAttributes (Just "rid") (Just "eid") (Just "type") LocalName)
                    "span"
    internalized = (internalize'
                    xmlContents
                    (filter isElementP xml)
                    (makeAttributiveRanges annotations)
                    tagSerializer)
  -- assert that there is valid xml. We use hxt's parser for this.
  return $ runParser document' (withNormNewline ()) ("("++sampleXml++")") internalized
  where
    sampleELisp = sampleXml ++ ".dump.el"

test_herder' = do
  sample <- validateInternalized' "doc/examples/herder_plastik_1778.TEI-P5.xml"
  assertBool(isRight sample)

test_rosenkranz' = do
  sample <- validateInternalized' "doc/examples/rosenkranz_aesthetik_1853.TEI-P5.xml"
  assertBool(isRight sample)
