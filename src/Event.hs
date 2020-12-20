{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Event where

import Prelude hiding (unlines)
import Lens.Micro
import Data.Maybe (fromJust)
import Data.List hiding (unlines)
import Data.Text (Text, unlines)

import Brick.Focus (FocusRing, focusGetCurrent, focusNext, focusPrev)
import Brick.Widgets.Edit (Editor, editorText, getEditContents, handleEditorEvent)
import Brick.Types (BrickEvent, BrickEvent(..), EventM, Next, handleEventLensed)
import Brick.Main (continue, halt)
import Graphics.Vty (Event(EvKey), Key(KChar, KBackTab), Modifier(MCtrl))

import State
import Prim
import qualified Field
import Field (Field(..))
import qualified Note
import Note (Note, Note(..), locked)


isEditing :: St -> Bool
isEditing st =
    let  foc = focusGetCurrent (st ^. focusRing)
         unlocked = filter (\note -> (not . _locked) note && (== Just (_id note)) foc) (st ^. notes)
     in (not . null) unlocked

unlockActive :: FocusRing Id -> [Note] -> [Note]
unlockActive foc = map (\note@(Note i a l t field@(Field c _) _) ->
                        if (== Just i) (focusGetCurrent foc)
                        then note & Note.content . Field.editor .~ editorText i Nothing c & locked .~ False
                        else note)

lockActive :: FocusRing Id -> [Note] -> [Note]
lockActive foc = map (\note@(Note i a l t field@(Field c e) _) ->
                        if (== Just i) (focusGetCurrent foc)
                           then (note & Note.content . Field.content .~ unlines (getEditContents e)) & locked .~ True
                        else note)

updateUnlockedEditor :: Functor f => (Editor Text Id -> f (Editor Text Id)) -> St -> f St
updateUnlockedEditor f st =
    let unlockedNote = fromJust $ find (not . _locked) (st ^. notes)
     in (\editor' -> st & notes %~ map (updateEditor editor')) <$> f (Field._editor (Note._content unlockedNote))
        where updateEditor ed note | (not . _locked) note = note & Note.content . Field.editor .~ ed
              updateEditor ed note = note

-- Editor has C-e, C-a, C-d, C-k, C-u, arrows, enter, paste. Should probably
-- keep away from those
eventHandler :: St -> BrickEvent Id e -> EventM Id (Next St)
eventHandler st (VtyEvent ev)  =
     case ev of
        EvKey (KChar '\t') [] -> continue (st & focusRing %~ focusNext)
        EvKey KBackTab [] -> continue (st & focusRing %~ focusPrev)
        EvKey (KChar 'o') [MCtrl] -> continue (st & notes %~ unlockActive (st ^. focusRing))
        EvKey (KChar 's') [MCtrl] -> continue (st & notes %~ lockActive (st ^. focusRing))
        EvKey (KChar 'g') [MCtrl] | isEditing st -> continue (st & notes %~ lockActive (st ^. focusRing))
        EvKey (KChar 'g') [MCtrl] -> halt st
        EvKey (KChar 'c') [MCtrl] -> halt st
        _ | isEditing st -> continue =<< handleEventLensed st updateUnlockedEditor handleEditorEvent ev
        _ -> continue st
eventHandler st _ = continue st

