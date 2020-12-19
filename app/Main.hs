{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Lens.Micro
import Event (eventHandler)
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( (<=>), str, txt )
import Brick.Util ( on)
import Database (serialize, deserialize)
import Text.Pretty.Simple (pString)
import Data.Text.Lazy (toStrict)
import Note (Note, Note(..))
import qualified Graphics.Vty as V
import qualified Note
import Prim
import State


dbdir = "db"
-- dbfile = "db/large.json"
dbfile = "db/database.json"

-------------------------------------------------------------------------------
-- Rendering
--

prettyRender :: [Note] -> T.Widget Id
prettyRender = txt . toStrict . pString . show

draw :: St -> [T.Widget Id]
draw st =
    Note.renderMany (st^.focusRing) (st^.notes) :  [B.hBorderWithLabel (str "State") <=> C.center (prettyRender (st^.notes))]

-------------------------------------------------------------------------------
--  Event handler

appCursor :: St -> [T.CursorLocation Id] -> Maybe (T.CursorLocation Id)
appCursor = F.focusRingCursor (^.focusRing)

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
            [ (E.editAttr, V.white `on` V.blue)
            , (E.editFocusedAttr, V.black `on` V.yellow)
            ]

theApp :: M.App St e Id
theApp =
    M.App { M.appDraw = draw
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = eventHandler
          , M.appStartEvent = return
          , M.appAttrMap = const attrMap
          }

initialState :: [Note] -> St
initialState notes = St (F.focusRing (map _id notes)) notes

main :: IO ()
main = do
    createDirectoryIfMissing True dbdir
    exists <- doesFileExist dbfile
    unless exists $  writeFile dbfile ""
    xs <- deserialize dbfile
    case xs of
      Nothing -> putStrLn "Notes.hs: Could not deserialize json"
      Just xs -> do
        st <- M.defaultMain theApp $ initialState xs
        writeFile dbfile ""
        serialize dbfile (st^.notes)
        putStrLn "Done"

