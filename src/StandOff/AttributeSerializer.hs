module StandOff.AttributeSerializer {-# DEPRECATED "Use StandOff.AttributeMap instead!" #-} where

import Data.List
import qualified Data.Map as Map
import Data.UUID.Types (toString)

import StandOff.AnnotationTypeDefs
import StandOff.TagTypeDefs
import StandOff.TagSerializer

-- very simple serializer
serializeAttributes' :: Annotation -> String
serializeAttributes' a =
  " elementId=\"" ++ (toString (rangeElementId a)) ++ "\""
  ++ " rangeId=\"" ++ (emptyStringWhenNothing (fmap toString (rangeRangeId a))) ++ "\""
  ++ (foldl (\acc (k, v) -> acc ++ " " ++ k ++ "=\"" ++ (intercalate " " v) ++ "\"")
       ""
       (Map.toList $ rangeAttributes a))
  where emptyStringWhenNothing Nothing = ""
        emptyStringWhenNothing (Just s) = s

-- configurable serializer
serializeAttributes :: (Maybe String) -> (Maybe String) -> (Maybe String) -> NSNameValueSerializer -> Annotation -> String
serializeAttributes rangeIdAttr elementIdAttr typeAttr typeValueSlizer a =
  (maybeAttribute rangeIdAttr $ fmap toString $ rangeRangeId a)
  ++ (maybeAttribute elementIdAttr $ Just $ toString $ rangeElementId a)
  ++ (maybeAttribute typeAttr $ Just $ nsNameValueSerializer typeValueSlizer $ rangeType a)
  ++ (foldl (\acc (k, v) -> acc ++ " " ++ k ++ "=\"" ++ (intercalate " " v) ++ "\"")
       ""
       (Map.toList $ rangeAttributes a))
  where emptyStringWhenNothing Nothing = ""
        emptyStringWhenNothing (Just s) = s
        maybeAttribute (Just key) (Just val) = " " ++ key ++ "=\"" ++ val ++ "\""
        maybeAttribute Nothing _ = ""
        maybeAttribute _ Nothing = ""
