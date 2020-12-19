{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module Database where

import Prelude hiding (writeFile)
import qualified Note as N
import Field
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import qualified Brick.Widgets.Edit as E
import Data.ByteString.Lazy (toStrict)
import qualified Brick.Focus as F
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.Generics
import Data.Text (Text)
import Data.Text.IO (writeFile)

data Note = Note { _id :: Integer, _active :: Bool, _locked :: Bool,  _title :: Text, _content :: Text }
    deriving (Generic, Show)

instance FromJSON Note

instance ToJSON Note

internalize :: N.Note -> Note
internalize (N.Note id active locked (Field title _) (Field content _) _) = Note id active locked title content

notesToString :: [N.Note] -> Text
notesToString = decodeUtf8 . toStrict  . encode . map internalize

serialize :: FilePath -> [N.Note] -> IO ()
serialize file xs = writeFile file (notesToString xs)

extractList :: Maybe [a] -> [a]
extractList Nothing = []
extractList (Just xs) = xs

externalize :: Note -> N.Note
externalize (Note id active locked title content) =
    N.Note id active locked (Field title (E.editor id Nothing title)) (Field content (E.editor id Nothing content)) (F.focusRing [N.Title, N.Content])

deserialize :: FilePath -> IO (Maybe [N.Note])
deserialize file = do
    content <- readFile file
    return $ map externalize <$> decode (fromString content)

