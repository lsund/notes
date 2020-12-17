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
import Note (Note, Note(..), Field(..))
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
-- dbfile = "db/large.json"
dbfile = "db/database.json"

type Id = Integer

data St =
    St { _focusRing :: F.FocusRing Id
       , _notes :: [Note]
       }

makeLenses ''St

-------------------------------------------------------------------------------
-- Rendering
--

prettyRender :: [Note] -> T.Widget Id
prettyRender = txt . toStrict . pString . show

draw :: St -> [T.Widget Id]
draw st = [Note.renderMany (st^.focusRing) (st^.notes)
            <=> B.hBorderWithLabel (str "State")
            <=> C.center (prettyRender (st^.notes))]

-------------------------------------------------------------------------------
--  Event handler

unlockActive :: F.FocusRing Id -> [Note] -> [Note]
unlockActive foc = map (\note@(Note i a l t field@(Field c _)) ->
                        if (== Just i) (F.focusGetCurrent foc)
                        then note { _locked = False, _content = field { _editor = E.editorText (_id note) Nothing c}}
                        else note)

lockActive :: F.FocusRing Id -> [Note] -> [Note]
lockActive foc = map (\note@(Note i a l t field@(Field c e)) ->
                        if (== Just i) (F.focusGetCurrent foc)
                           then note { _locked = True, _content = field { _fcontent = unlines $ E.getEditContents e}}
                        else note)

updateUnlockedEditor :: Functor f => (E.Editor Text Id -> f (E.Editor Text Id)) -> St -> f St
updateUnlockedEditor f st =
    let xs = (st ^. notes)
        unlockedNote = fromJust $ find (not . _locked) xs
     in (\editor' -> st {_notes = map (updateEditor editor') xs }) <$> f (_editor (_content unlockedNote))
        where updateEditor ed note | (not . _locked) note = note { _content = (_content note) { _editor = ed } }
              updateEditor ed note = note

appEvent :: St -> T.BrickEvent Id e -> T.EventM Id (T.Next St)
appEvent st (T.VtyEvent ev)  =
     case ev of
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'd') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'p') [V.MCtrl] -> M.continue (st & focusRing %~ F.focusPrev)
        V.EvKey (V.KChar 'n') [V.MCtrl] -> M.continue (st & focusRing %~ F.focusNext)
        V.EvKey (V.KChar 'e') [V.MCtrl] -> M.continue (st & notes %~ unlockActive (st ^. focusRing))
        V.EvKey (V.KChar 's') [V.MCtrl] -> M.continue (st & notes %~ lockActive (st ^. focusRing))
        V.EvKey (V.KChar 'g') [V.MCtrl] -> M.continue (st & notes %~ lockActive (st ^. focusRing))
        _ -> M.continue =<< T.handleEventLensed st updateUnlockedEditor E.handleEditorEvent ev
appEvent st _ = M.continue st

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
    St
        (F.focusRing (map _id (x : xs)))
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

