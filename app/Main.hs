{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Main where

import           Control.Monad        (unless)
import           Data.Text.Lazy       (toStrict)
import           Lens.Micro
import           System.Directory     (createDirectoryIfMissing, doesFileExist)
import           Text.Pretty.Simple   (pString)
import Data.List (find)
import Data.Maybe (fromJust)

import           Brick.AttrMap        (attrMap)
import qualified Brick.Focus          as Focus
import           Brick.Main           (App (..), appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent,
                                       defaultMain)
import           Brick.Types          (CursorLocation, Widget)
import           Brick.Util           (on, fg)
import           Brick.Widgets.Border (hBorderWithLabel)
import           Brick.Widgets.Center (center)
import           Brick.Widgets.Core   (str, txt, (<=>))
import           Brick.Widgets.Edit   (editAttr, editFocusedAttr)
import           Graphics.Vty         (black, blue, defAttr, white, yellow, green, cyan)

import           Database             (deserialize, serialize)
import           Event                (eventHandler)
import           Note                 (Note (..), focusRing)
import qualified Note
import           Resource
import           State


dbdir :: FilePath
dbdir = "db"

dbfile :: FilePath
-- dbfile = "db/large.json"
dbfile = "db/database.json"

-------------------------------------------------------------------------------
-- Rendering
--

prettyRender :: [Note] -> Widget Resource
prettyRender = txt . toStrict . pString . show

draw :: St -> [Widget Resource]
draw st =
    Note.renderMany (st^.notes) :  [hBorderWithLabel (str "State") <=> center (prettyRender (st^.notes))]

-------------------------------------------------------------------------------
--  Event handler

appCursor :: St -> [CursorLocation Resource] -> Maybe (CursorLocation Resource)
appCursor st xs | null (find (not . _locked) (st ^. notes)) = Nothing
appCursor st xs = Focus.focusRingCursor (^. notes . to (fromJust . find (not . _locked)) . focusRing) st xs

-- App s e n
--
-- s: The application state. Will evolve during execution. Defined in State.hs
-- e: Event type.
-- n: Resource name type: Used to uniquely identify Widgets
--
theApp :: App St e Resource
theApp =
    App { appDraw = draw
        , appChooseCursor = appCursor
        , appHandleEvent = eventHandler
        , appStartEvent = return
        , appAttrMap =
            const $ attrMap defAttr
                        [ (editAttr, white `on` blue)
                        , (editFocusedAttr, black `on` yellow)
                        , ("meta", green `on` black)
                        , ("title", fg cyan)]
        }

initialState :: [Note] -> St
initialState = St

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

