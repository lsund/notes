{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Lens.Micro
import Text.Pretty.Simple (pString)
import Data.Text.Lazy (toStrict)

import qualified Brick.Focus as Focus
import Brick.Util (on)
import Brick.AttrMap (attrMap)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Brick.Main (App, App(..), appDraw, appChooseCursor, appHandleEvent, appStartEvent, appAttrMap, defaultMain)
import Brick.Types (Widget, CursorLocation)
import Brick.Widgets.Border (hBorderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core ((<=>), str, txt)
import Graphics.Vty (defAttr, white, blue, black, yellow)

import Prim
import State
import Event (eventHandler)
import Database (serialize, deserialize)
import Note (Note, Note(..))
import qualified Note


dbdir = "db"
-- dbfile = "db/large.json"
dbfile = "db/database.json"

-------------------------------------------------------------------------------
-- Rendering
--

prettyRender :: [Note] -> Widget Id
prettyRender = txt . toStrict . pString . show

draw :: St -> [Widget Id]
draw st =
    Note.renderMany (st^.focusRing) (st^.notes) :  [hBorderWithLabel (str "State") <=> center (prettyRender (st^.notes))]

-------------------------------------------------------------------------------
--  Event handler

appCursor :: St -> [CursorLocation Id] -> Maybe (CursorLocation Id)
appCursor = Focus.focusRingCursor (^.focusRing)

theApp :: App St e Id
theApp =
    App { appDraw = draw
        , appChooseCursor = appCursor
        , appHandleEvent = eventHandler
        , appStartEvent = return
        , appAttrMap = const $ attrMap defAttr [ (editAttr, white `on` blue) , (editFocusedAttr, black `on` yellow) ]
        }

initialState :: [Note] -> St
initialState notes = St (Focus.focusRing (map _id notes)) notes

main :: IO ()
main = do
    createDirectoryIfMissing True dbdir
    exists <- doesFileExist dbfile
    unless exists $  writeFile dbfile ""
    xs <- deserialize dbfile
    case xs of
      Nothing -> putStrLn "Notes.hs: Could not deserialize json"
      Just xs -> do
        st <- defaultMain theApp $ initialState xs
        writeFile dbfile ""
        serialize dbfile (st^.notes)
        putStrLn "Done"

