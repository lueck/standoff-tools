module StandOff.Extract
where

import StandOff.TextRange
import StandOff.StringLike (StringLike)
import qualified StandOff.StringLike as S


-- | Extract an annotated span from a source string.
extractAnnotation
  :: (StringLike s, TextRange a) =>
     s -- ^ the input source file the annotation references a span of
  -> a -- ^ annotation
  -> s -- ^ returns the annotated span
extractAnnotation src annot = S.take l $ S.drop strt src
  where
    strt = start annot
    l = (end annot) - strt + 1
