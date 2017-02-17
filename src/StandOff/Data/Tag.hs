module StandOff.Data.Tag where

data TagType = Open | Close | Empty deriving (Eq, Show)

data NSNameValueSerializer = FullName | LocalName
