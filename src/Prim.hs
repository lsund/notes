module Prim where

import           Data.Char        (isAlphaNum, isPunctuation)
import           Data.Text        (Text, dropWhile, dropWhileEnd, pack)
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Prelude          hiding (dropWhile)

data Direction = Left | Right deriving (Eq)

data FieldName = Title | Content deriving (Show, Eq, Ord)

noteHeight :: Bool -> Int
noteHeight False = 10
noteHeight True = 20

noteWidth :: Bool -> Int
noteWidth False = 40
noteWidth True  = 60

formatStr :: String
formatStr = "%d-%m-%Y %l:%M %p"

unparseTime :: UTCTime -> Text
unparseTime = pack . formatTime defaultTimeLocale formatStr

strip :: Text -> Text
strip = dropWhile (not . allowed) . dropWhileEnd (not . allowed)
    where allowed x = isAlphaNum x || isPunctuation x

