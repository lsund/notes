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
import Note (Note, Note(..), Field(..), editor, fcontent, content, locked)
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
draw st =
    Note.renderMany (st^.focusRing) (st^.notes) :  [B.hBorderWithLabel (str "State") <=> C.center (prettyRender (st^.notes))]

-------------------------------------------------------------------------------
--  Event handler

isEditing :: St -> Bool
isEditing st =
    let  foc = F.focusGetCurrent (st ^. focusRing)
         unlocked = filter (\note -> (not . _locked) note && (== Just (_id note)) foc) (st ^. notes)
     in (not . null) unlocked

unlockActive :: F.FocusRing Id -> [Note] -> [Note]
unlockActive foc = map (\note@(Note i a l t field@(Field c _)) ->
                        if (== Just i) (F.focusGetCurrent foc)
                        then note & content . editor .~ E.editorText i Nothing c & locked .~ False
                        else note)

lockActive :: F.FocusRing Id -> [Note] -> [Note]
lockActive foc = map (\note@(Note i a l t field@(Field c e)) ->
                        if (== Just i) (F.focusGetCurrent foc)
                           then (note & content . fcontent .~ unlines (E.getEditContents e)) & locked .~ True
                        else note)

updateUnlockedEditor :: Functor f => (E.Editor Text Id -> f (E.Editor Text Id)) -> St -> f St
updateUnlockedEditor f st =
    let unlockedNote = fromJust $ find (not . _locked) (st ^. notes)
     in (\editor' -> st & notes %~ map (updateEditor editor')) <$> f (_editor (_content unlockedNote))
        where updateEditor ed note | (not . _locked) note = note & content . editor .~ ed
              updateEditor ed note = note

-- Editor has C-e, C-a, C-d, C-k, C-u, arrows, enter, paste. Should probably
-- keep away from those
appEvent :: St -> T.BrickEvent Id e -> T.EventM Id (T.Next St)
appEvent st (T.VtyEvent ev)  =
     case ev of
        V.EvKey (V.KChar '\t') [] -> M.continue (st & focusRing %~ F.focusNext)
        V.EvKey V.KBackTab [] -> M.continue (st & focusRing %~ F.focusPrev)
        V.EvKey (V.KChar 'o') [V.MCtrl] -> M.continue (st & notes %~ unlockActive (st ^. focusRing))
        V.EvKey (V.KChar 's') [V.MCtrl] -> M.continue (st & notes %~ lockActive (st ^. focusRing))
        V.EvKey (V.KChar 'g') [V.MCtrl] | isEditing st -> M.continue (st & notes %~ lockActive (st ^. focusRing))
        V.EvKey (V.KChar 'g') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt st
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

