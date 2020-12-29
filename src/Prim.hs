module Prim where

import           Data.Char        (isAlphaNum, isPunctuation)
import           Data.Text        (Text, dropWhile, dropWhileEnd, pack)
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Prelude          hiding (dropWhile)

data Direction = Left | Right deriving (Eq)

data FieldName = Title | Content deriving (Show, Eq, Ord)

formatStr :: String
formatStr = "%d-%m-%Y %l:%M %p"

unparseTime :: UTCTime -> Text
unparseTime = pack . formatTime defaultTimeLocale formatStr

strip :: Text -> Text
strip = dropWhile (not . allowed) . dropWhileEnd (not . allowed)
    where allowed x = isAlphaNum x || isPunctuation x

