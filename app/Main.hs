{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Main where

import           Control.Monad        (unless)
import           Data.Text.Lazy       (toStrict)
import           Lens.Micro
import           System.Directory     (createDirectoryIfMissing, doesFileExist)
import           Text.Pretty.Simple   (pString)

import           Brick.AttrMap        (attrMap)
import qualified Brick.Focus          as Focus
import           Brick.Main           (App (..), appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent,
                                       defaultMain)
import           Brick.Types          (CursorLocation, Widget)
import           Brick.Util           (on)
import           Brick.Widgets.Border (hBorderWithLabel)
import           Brick.Widgets.Center (center)
import           Brick.Widgets.Core   (str, txt, (<=>))
import           Brick.Widgets.Edit   (editAttr, editFocusedAttr)
import           Graphics.Vty         (black, blue, defAttr, white, yellow)

import           Database             (deserialize, serialize)
import           Event                (eventHandler)
import           Note                 (Note (..))
import qualified Note
import           Prim
import           State


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

