{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Lens.Micro
import Lens.Micro.TH
import qualified Note
import Note (Note, Note(..), Content(..))
import Shared (Name, Name(..))
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util ( on)
import Database (serialize, deserialize)

dbdir = "db"
dbfile = "db/database.dhall"

data St =
    St { _focusRing :: F.FocusRing Name
       , _editTitle :: E.Editor String Name
       , _editContent :: E.Editor String Name
       , _currentResource :: Name
       , _notes :: [Note]
       }

makeLenses ''St

-------------------------------------------------------------------------------
-- Rendering

draw :: St -> [T.Widget Name]
draw st | viewMode st = [Note.renderMany (st^.notes)]
draw st | editMode st = [Note.renderEditable (st^.focusRing) (st^.editTitle) (st^.editContent)]


nextMode :: Name -> Name
nextMode View1 = EditTitle
nextMode _ = View1

editMode :: St -> Bool
editMode st = (st^.currentResource) == EditTitle || (st^.currentResource) == EditContent

viewMode :: St -> Bool
viewMode st = (st^.currentResource) == View1

-------------------------------------------------------------------------------
--  Event handler

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) | viewMode st =
    case ev of
        V.EvKey (V.KChar 'g') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'n') [V.MCtrl] -> M.continue (st&currentResource %~ nextMode)
        _  -> M.continue st
appEvent st (T.VtyEvent ev) | editMode st =
    case ev of
        V.EvKey (V.KChar 'g') [V.MCtrl] -> M.continue (st&currentResource %~ nextMode)
        V.EvKey (V.KChar 's') [V.MCtrl] ->
            let title = (unlines $ E.getEditContents $ st^.editTitle)
                content = (unlines $ E.getEditContents $ st^.editContent)
             in M.continue ((st&currentResource %~ nextMode) &notes %~ (\xs -> Note title (FreeText content) : xs) )
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just EditTitle -> T.handleEventLensed st editTitle E.handleEditorEvent ev
               Just EditContent -> T.handleEventLensed st editContent E.handleEditorEvent ev
               Nothing -> return st
appEvent st _ = M.continue st

-------------------------------------------------------------------------------
-- Init

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
            [ (E.editAttr, V.white `on` V.blue)
            , (E.editFocusedAttr, V.black `on` V.yellow)
            ]

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = draw
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const attrMap
          }

initialState :: [Note] -> St
initialState =
    St (F.focusRing [EditTitle, EditContent])
       (E.editor EditTitle (Just 1) "")
       (E.editor EditContent Nothing "")
       View1

main :: IO ()
main = do
    createDirectoryIfMissing True dbdir
    exists <- doesFileExist dbfile
    unless exists $  writeFile dbfile ""
    xs <- deserialize dbfile
    st <- M.defaultMain theApp $ initialState xs
    writeFile dbfile ""
    serialize dbfile (st^.notes)
    putStrLn "Done"

