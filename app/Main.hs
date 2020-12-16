{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Prelude hiding (unlines)
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Lens.Micro
import Lens.Micro.TH
import qualified Note
import Note (Note, Note(..))
import Shared (Name, Name(..))
import qualified Graphics.Vty as V
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
import Data.List hiding (unlines)
import Text.Pretty.Simple (pString)
import Data.Text.Lazy (toStrict)
import Data.Text (Text, unlines)


dbdir = "db"
dbfile = "db/database.json"

data St =
    St { _focusRing :: F.FocusRing Name
       , _editTitle :: E.Editor Text Name
       , _editContent :: E.Editor Text Name
       , _currentResource :: Name
       , _notes :: [Note]
       }

makeLenses ''St

-------------------------------------------------------------------------------
-- Rendering
--

prettyRender :: [Note] -> T.Widget Name
prettyRender = txt . toStrict . pString . show

draw :: St -> [T.Widget Name]
draw st = [Note.renderMany (st^.focusRing) (st^.editTitle) (st^.editContent) (st^.notes)
            <=> B.hBorderWithLabel (str "State")
            <=> C.center (prettyRender (st^.notes))]


nextMode :: Name -> Name
nextMode View1 = EditTitle
nextMode _ = View1

editMode :: St -> Bool
editMode st = (st^.currentResource) == EditTitle || (st^.currentResource) == EditContent

viewMode :: St -> Bool
viewMode st = (st^.currentResource) == View1

activateOnId :: Integer -> [Note] -> [Note]
activateOnId id  = map (\note -> if _id note == id then note { _active = True } else note { _active = False })

activate :: (Integer -> Integer) -> [Note] -> [Note]
activate next xs =
    let maxIndex = maximum $ map _id xs
     in case find _active xs of
      Nothing -> undefined
      Just activeNote ->
          let nextIndex = max (min (next (_id activeNote)) maxIndex) 0
           in activateOnId nextIndex xs

unlockActive :: [Note] -> [Note]
unlockActive = map (\note -> if _active note then note { _locked = False } else note)

lockActive :: [Note] -> [Note]
lockActive = map (\note -> if _active note then note { _locked = True } else note)

-------------------------------------------------------------------------------
--  Event handler

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.VtyEvent ev) | viewMode st =
     case ev of
        V.EvKey (V.KChar 'g') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'd') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'p') [V.MCtrl] -> M.continue (st&notes %~ activate pred)
        V.EvKey (V.KChar 'n') [V.MCtrl] -> M.continue (st&notes %~ activate succ)
        V.EvKey (V.KChar 'e') [V.MCtrl] -> M.continue (st&notes %~ unlockActive)
        V.EvKey (V.KChar 's') [V.MCtrl] -> M.continue (st&notes %~ lockActive)
        _  -> M.continue st
appEvent st (T.VtyEvent ev) | editMode st =
    case ev of
        V.EvKey (V.KChar 'g') [V.MCtrl] -> M.continue (st&currentResource %~ nextMode)
        V.EvKey (V.KChar 's') [V.MCtrl] ->
            let title = (unlines $ E.getEditContents $ st^.editTitle)
                content = (unlines $ E.getEditContents $ st^.editContent)
                withNextMode = st&currentResource%~nextMode
                nextId = succ (maximum (map _id (st^.notes)))
                withNewNoteAdded = withNextMode&notes%~(\notes -> Note nextId False True title [content] : notes)
             in M.continue withNewNoteAdded
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
initialState (x : xs) =
    St (F.focusRing [EditTitle, EditContent])
       (E.editor EditTitle (Just 1) "") -- initialize editors
       (E.editor EditContent Nothing "") -- Initialize editors
       View1
       (x : xs)



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

