{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Database where

import           Data.Aeson           (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Lazy (toStrict)
import           Data.String          (fromString)
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Text.IO         (writeFile)
import           GHC.Generics
import           Prelude              hiding (writeFile)

import qualified Brick.Focus          as Focus
import qualified Brick.Widgets.Edit   as Edit

import           Field
import           Note

data SerializedNote = SerializedNote
                        { _id      :: Integer
                        , _active  :: Bool
                        , _locked  :: Bool
                        , _title   :: Text
                        , _content :: Text
                        }
  deriving (Generic, Show)

instance FromJSON SerializedNote

instance ToJSON SerializedNote

internalize :: Note -> SerializedNote
internalize (Note id active locked (Field title _) (Field content _) _) = SerializedNote id active locked title content

notesToString :: [Note] -> Text
notesToString = decodeUtf8 . toStrict  . encode . map internalize

serialize :: FilePath -> [Note] -> IO ()
serialize file xs = writeFile file (notesToString xs)

extractList :: Maybe [a] -> [a]
extractList Nothing   = []
extractList (Just xs) = xs

externalize :: SerializedNote -> Note
externalize (SerializedNote id active locked title content) =
    Note id active locked (Field title (Edit.editor id Nothing title)) (Field content (Edit.editor id Nothing content)) (Focus.focusRing [Title, Content])

deserialize :: FilePath -> IO (Maybe [Note])
deserialize file = do
    content <- readFile file
    return $ map externalize <$> decode (fromString content)

