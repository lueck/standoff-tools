module StandOff.TagTypeDefs where

-- | Tag types for use in internalize.
data TagType
  = Open
  | Close
  | Empty
  deriving (Eq, Show)

data NSNameValueSerializer
  = FullName
  | LocalName
