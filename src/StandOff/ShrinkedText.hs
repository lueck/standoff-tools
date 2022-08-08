module ShrinkedText
where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Tree.Class


import StandOff.LineOffsets
import StandOff.StringLike
import StandOff.XTraverse


-- | A mapping of offsets in the XML file to offsets in the generated
-- plain text file. The list index represents the XML offset, the
-- element value the offset in the plain text file.
data OffsetMapping = OffsetMapping
  { offsetMap_offsetMapping :: [Int] -- ^ mapping of character offsets
  , offsetMap_byteMapping :: [Int]   -- ^ mapping of byte offsets
  }

initialOffsetMapping :: OffsetMapping
initialOffsetMapping = OffsetMapping [] []


-- | A config of node qnames to replacement strings
type ShrinkingNodeConfig = Map.Map String String


class ShrinkingNode n where
  -- | This processes an opening part of a node (in case of an element
  -- node) or the complete node
  shrinkOpen
    :: (StringLike s) =>
    ShrinkingNodeConfig -- ^ a configuration
    -> n                -- ^ the node
    -> s                -- ^ the input string from this node till EOF
    -> OffsetMapping    -- ^ the the offset mapping generated so far
    -> (s, (s, OffsetMapping)) -- ^ returns a tupel of output string
                               -- and new state of left input string
                               -- and new offset mapping

  -- | This processes the closing part of a node. The arguments are
  -- the same as in 'serializeOpen'.
  shrinkClose
    :: (StringLike s) =>
       ShrinkingNodeConfig
    -> n
    -> s
    -> OffsetMapping
    -> (s, (s, OffsetMapping))


-- | Generate shrinked text.
shrinkedText
  :: (StringLike s, Monoid s, Monad m, Tree t, ShrinkingNode n) =>
     (s -> m ())  -- ^ monadic action
  -> ShrinkingNodeConfig
  -> [t n]        -- ^ the parsed XML document
  -> s            -- ^ the XML document as a string
  -> m (OffsetMapping)
shrinkedText writeM nodeCfg xml s = do
  final <- execStateT (mapM_ (xtraverse_ writeM f g) xml) (s, initialOffsetMapping)
  -- We cannot write what is left over, because we do not have a
  -- mapping for it. Should we do a test on the final state or write
  -- what is left over?
  --
  -- writeM $ fst final
  return $ snd final
  where
    f n = uncurry (shrinkOpen nodeCfg n)
    g n = uncurry (shrinkClose nodeCfg n)


