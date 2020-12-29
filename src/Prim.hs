module Prim where

import           Data.Char        (isAlphaNum)
import           Data.Text        (Text, dropWhile, dropWhileEnd, pack)
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Prelude          hiding (dropWhile)

data FieldName = Title | Content deriving (Show, Eq, Ord)

formatStr :: String
formatStr = "%d-%m-%Y %l:%M %p"

unparseTime :: UTCTime -> Text
unparseTime = pack . formatTime defaultTimeLocale formatStr

strip :: Text -> Text
strip = dropWhile (not . isAlphaNum) . dropWhileEnd (not . isAlphaNum)

