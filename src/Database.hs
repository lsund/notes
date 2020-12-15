{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RankNTypes #-}
module Database where

import qualified Note as N;
import Dhall hiding (map)

data Note = Note { title :: String, bullets :: [String] }
    deriving (Generic)

instance FromDhall Note

data Database = Database { notes :: [Note] }
    deriving (Generic)

instance FromDhall Database

instance Show Note where
    show (Note title bullets) = "{title=" <> show title <> ",bullets=" <> show bullets <> "}"

instance Show Database where
    show (Database notes) = "{notes=" <> show notes <> "}"

serializeNote :: N.Note -> Note
serializeNote (N.Note title (N.Bullets xs)) = Note title xs

serialize :: FilePath -> [N.Note] -> IO ()
serialize file xs = writeFile file (show (Database (map serializeNote xs)))

deserializeNote :: Note -> N.Note
deserializeNote (Note title xs) = N.Note title (N.Bullets xs)


deserialize :: FilePath -> IO [N.Note]
deserialize file = do
    database <- input auto "./db/database.dhall"
    print $ (database :: Database)
    return $ map deserializeNote (notes database)

