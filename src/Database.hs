{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Database where

import Note (Note(..))
import Data.Aeson (decode, encode)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)
import Data.ByteString.Lazy (toStrict)

notesToString :: [Note] -> String
notesToString = unpack . decodeUtf8 . toStrict  . encode

serialize :: FilePath -> [Note] -> IO ()
serialize file xs = writeFile file (notesToString xs)

extractList :: Maybe [a] -> [a]
extractList Nothing = []
extractList (Just xs) = xs

deserialize :: FilePath -> IO [Note]
deserialize file = do
    content <- readFile file
    return $ (extractList . decode) (fromString content)

