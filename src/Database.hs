{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Database where

import Note (Note, Note(..))
import Data.List.Split
import Data.Maybe (mapMaybe)

serialize :: FilePath -> [Note] -> IO ()
serialize file = do
    let _ = writeFile file ""
    mapM_ (appendFile file . show)

deserializeNote :: [String] -> Maybe Note
deserializeNote [x, y] = Just $ Note x y
deserializeNote _ = Nothing


deserialize :: FilePath -> IO [Note]
deserialize file = do
    contents <- readFile file
    return $ mapMaybe deserializeNote (chunksOf 2 (splitOn "#" contents))


