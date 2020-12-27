module Prim where

import           Data.Text        (Text, pack)
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)

data FieldName = Title | Content deriving (Show, Eq, Ord)

formatStr :: String
formatStr = "%d-%m-%Y %l:%M %p"

unparseTime :: UTCTime -> Text
unparseTime = pack . formatTime defaultTimeLocale formatStr
