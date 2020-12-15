{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Database where

import Note (Note(..))
import Data.Aeson (decode, encode)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)
import Data.ByteString.Lazy (toStrict)

serialize :: FilePath -> [Note] -> IO ()
serialize file xs = writeFile file (unpack (decodeUtf8 (toStrict  (encode xs))))

deserialize :: FilePath -> IO [Note]
deserialize file = do
    content <- readFile file
    case decode (fromString content) of
        Nothing -> return []
        Just xs -> return xs

