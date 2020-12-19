{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Event where

import State
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import Prim
import qualified Field
import qualified Note
import Field (Field(..))
import Note (Note, Note(..), locked)
import Data.Text (Text, unlines)
import Prelude hiding (unlines)
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import Lens.Micro
import Data.Maybe (fromJust)
import Data.List hiding (unlines)

isEditing :: St -> Bool
isEditing st =
    let  foc = F.focusGetCurrent (st ^. focusRing)
         unlocked = filter (\note -> (not . _locked) note && (== Just (_id note)) foc) (st ^. notes)
     in (not . null) unlocked

unlockActive :: F.FocusRing Id -> [Note] -> [Note]
unlockActive foc = map (\note@(Note i a l t field@(Field c _) _) ->
                        if (== Just i) (F.focusGetCurrent foc)
                        then note & Note.content . Field.editor .~ E.editorText i Nothing c & locked .~ False
                        else note)

lockActive :: F.FocusRing Id -> [Note] -> [Note]
lockActive foc = map (\note@(Note i a l t field@(Field c e) _) ->
                        if (== Just i) (F.focusGetCurrent foc)
                           then (note & Note.content . Field.content .~ unlines (E.getEditContents e)) & locked .~ True
                        else note)

updateUnlockedEditor :: Functor f => (E.Editor Text Id -> f (E.Editor Text Id)) -> St -> f St
updateUnlockedEditor f st =
    let unlockedNote = fromJust $ find (not . _locked) (st ^. notes)
     in (\editor' -> st & notes %~ map (updateEditor editor')) <$> f (Field._editor (Note._content unlockedNote))
        where updateEditor ed note | (not . _locked) note = note & Note.content . Field.editor .~ ed
              updateEditor ed note = note

-- Editor has C-e, C-a, C-d, C-k, C-u, arrows, enter, paste. Should probably
-- keep away from those
eventHandler :: St -> T.BrickEvent Id e -> T.EventM Id (T.Next St)
eventHandler st (T.VtyEvent ev)  =
     case ev of
        V.EvKey (V.KChar '\t') [] -> M.continue (st & focusRing %~ F.focusNext)
        V.EvKey V.KBackTab [] -> M.continue (st & focusRing %~ F.focusPrev)
        V.EvKey (V.KChar 'o') [V.MCtrl] -> M.continue (st & notes %~ unlockActive (st ^. focusRing))
        V.EvKey (V.KChar 's') [V.MCtrl] -> M.continue (st & notes %~ lockActive (st ^. focusRing))
        V.EvKey (V.KChar 'g') [V.MCtrl] | isEditing st -> M.continue (st & notes %~ lockActive (st ^. focusRing))
        V.EvKey (V.KChar 'g') [V.MCtrl] -> M.halt st
        V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt st
        _ | isEditing st -> M.continue =<< T.handleEventLensed st updateUnlockedEditor E.handleEditorEvent ev
        _ -> M.continue st
eventHandler st _ = M.continue st

