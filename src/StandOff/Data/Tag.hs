module StandOff.Data.Tag where

data TagType = Open | Close | Empty deriving (Show)

data NSNameValueSerializer = FullName | LocalName
