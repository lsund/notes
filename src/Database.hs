{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Database where

import           Data.Aeson           (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Lazy (toStrict)
import           Data.String          (fromString)
import           Data.Text            (Text, unpack, pack)
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Text.IO         (writeFile)
import           Data.Time.Clock      (UTCTime)
import           Data.Time.Format     (parseTimeOrError, defaultTimeLocale, formatTime)
import           GHC.Generics
import           Prelude              hiding (writeFile)

import qualified Brick.Focus          as Focus
import qualified Brick.Widgets.Edit   as Edit

import           Field
import           Note
import           Prim
import           Resource

data SerializedNote = SerializedNote
                        { _id          :: Int
                        , _active      :: Bool
                        , _locked      :: Bool
                        , _title       :: Text
                        , _content     :: Text
                        , _lastUpdated :: Text
                        }
  deriving (Generic, Show)

instance FromJSON SerializedNote
instance ToJSON SerializedNote

-------------------------------------------------------------------------------
-- From db

load :: SerializedNote -> Note
load (SerializedNote id active locked title content lastUpdated) =
    Note
        id
        active
        locked
        (Field title (Edit.editor (Resource id Title) Nothing title))
        (Field content (Edit.editor (Resource id Content) Nothing content))
        (Focus.focusRing [Resource id Title, Resource id Content])
        (parseTimeOrError True defaultTimeLocale formatStr (unpack lastUpdated) :: UTCTime)

deserialize :: FilePath -> IO (Maybe [Note])
deserialize file = do
    content <- readFile file
    return $ map load <$> decode (fromString content)

-------------------------------------------------------------------------------
-- To db

unload :: Note -> SerializedNote
unload (Note id active locked (Field title _) (Field content _) _ lastUpdated) =
    SerializedNote id active locked title content (pack (formatTime defaultTimeLocale formatStr lastUpdated))

encodeNotes :: [Note] -> Text
encodeNotes = decodeUtf8 . toStrict  . encode . map unload

serialize :: FilePath -> [Note] -> IO ()
serialize file xs = writeFile file (encodeNotes xs)
