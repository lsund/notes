{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Maybe (fromJust)
import Prelude hiding (unlines)
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Lens.Micro
import Lens.Micro.TH
import qualified Note
import Note (Note, Note(..), titleEditor)
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
import Data.Text (Text)


dbdir = "db"
-- dbfile = "db/large.json"
dbfile = "db/database.json"

type Id = Integer

data St =
    St { _focusRing :: F.FocusRing Id
       , _editTitle :: E.Editor Text Id
       , _editContent :: E.Editor Text Id
       , _currentResource :: Id
       , _notes :: [Note]
       }

makeLenses ''St

-------------------------------------------------------------------------------
-- Rendering
--

prettyRender :: [Note] -> T.Widget Id
prettyRender = txt . toStrict . pString . show

draw :: St -> [T.Widget Id]
draw st = [Note.renderMany (st^.focusRing) (st^.editContent) (st^.notes)
            <=> B.hBorderWithLabel (str "State")
            <=> C.center (prettyRender (st^.notes))]

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

appEvent :: St -> T.BrickEvent Id e -> T.EventM Id (T.Next St)
appEvent st (T.VtyEvent ev)  =
     case ev of
        V.EvKey (V.KChar 'g') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'd') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'p') [V.MCtrl] -> M.continue (st&notes %~ activate pred)
        V.EvKey (V.KChar 'n') [V.MCtrl] -> M.continue (st&notes %~ activate succ)
        V.EvKey (V.KChar 'e') [V.MCtrl] -> M.continue (st&notes %~ unlockActive)
        V.EvKey (V.KChar 's') [V.MCtrl] -> M.continue (st&notes %~ lockActive)
        -- FIXME
        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
        -- If one node is unlocked:
        -- editContent :: (st -> st)
        -- 1. get that node, get its editor `editTitle`
                Just id ->
                    let handleEvent lens st' =
                            let unlockedNote = find (not . _locked) (st'^.notes)
                                editor = fromJust $ (titleEditor <$> unlockedNote) :: (E.Editor Text Id)
                            in editor
                    in T.handleEventLensed st handleEvent E.handleEditorEvent ev
                Nothing -> T.handleEventLensed st editContent E.handleEditorEvent ev
        --- Otherwise continue with state unchanged
appEvent st _ = M.continue st

-- _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of

-- V.EvKey (V.KChar 'g') [V.MCtrl] -> M.continue (st&currentResource %~ nextMode)
-- V.EvKey (V.KChar 's') [V.MCtrl] ->
--     let title = (unlines $ E.getEditContents $ st^.editTitle)
--         content = (unlines $ E.getEditContents $ st^.editContent)
--         withNextMode = st&currentResource%~nextMode
--         nextId = succ (maximum (map _id (st^.notes)))
--         withNewNoteAdded = withNextMode&notes%~(\notes -> Note nextId False True title [content] : notes)
--      in M.continue withNewNoteAdded
-- V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
-- V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
--        Nothing -> return st
-------------------------------------------------------------------------------
-- Init

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
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const attrMap
          }

initialState :: [Note] -> St
initialState (x : xs) =
    St (F.focusRing [1, 2])
       (E.editor 1 (Just 1) "") -- initialize editors
       (E.editor 2 Nothing "") -- Initialize editors
       1
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

