{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Event where

import           Data.List          hiding (unlines)
import           Data.Maybe         (fromJust)
import           Data.Text          (Text, unlines)
import           Lens.Micro
import           Prelude            hiding (unlines)

import           Brick.Focus        (focusGetCurrent)
import           Brick.Main         (continue, halt)
import           Brick.Types        (BrickEvent (..), EventM, Next, handleEventLensed)
import           Brick.Widgets.Edit (Editor, editorText, getEditContents, handleEditorEvent)
import           Graphics.Vty       (Event (EvKey), Key (KBackTab, KChar), Modifier (MCtrl))

import           Field              (Field (..))
import qualified Field
import           Note               (Note (..), locked, focusRing)
import qualified Note
import           Prim
import           Resource
import           State

activateOnId :: Integer -> [Note] -> [Note]
activateOnId id  = map (\note -> if _id note == id then note { _active = True } else note { _active = False })

activate :: (Integer -> Integer) -> [Note] -> [Note]
activate next xs =
    let maxIndex = maximum $ map _id xs
     in case find _active xs of
      Nothing -> error "Should not happen"
      Just activeNote ->
          let nextIndex = max (min (next (_id activeNote)) maxIndex) 0
           in activateOnId nextIndex xs

focusedField :: Functor f => Note -> (Field -> f Field) -> Note -> f Note
focusedField note = case focusGetCurrent (note ^. focusRing) of
                Just (Resource _ Title) -> Note.title
                _ -> Note.content

isEditing :: St -> Bool
isEditing st = (not . null) (filter (not . _locked) (st ^. notes))

unlockActive :: [Note] -> [Note]
unlockActive = map (\note@(Note i a l (Field t _) (Field c _) foc) ->
                        if a
                              then let tt = editorText (Resource i Title) Nothing t
                                       tc = editorText (Resource i Content) Nothing c
                                    in note & Note.title . Field.editor .~ tt & Note.content . Field.editor .~ tc & locked .~ False
                        else note)

lockActive :: [Note] -> [Note]
lockActive = map (\note@(Note i a l (Field t te) (Field c ce) foc) ->
                    if a
                          then let ce' = unlines (getEditContents ce)
                                   te' = unlines (getEditContents te)
                                in note & Note.title . Field.content .~ te' & Note.content . Field.content .~ ce' & locked .~ True
                        else note)

updateUnlockedEditor :: Functor f => (Editor Text Resource -> f (Editor Text Resource)) -> St -> f St
updateUnlockedEditor f st =
    let  editor = case focus of
                   Just (Resource _ Title) -> Field._editor (Note._title unlockedNote)
                   _                       -> Field._editor (Note._content unlockedNote)
     in (\editor' -> st & notes %~ map (updateEditor editor')) <$> f editor
     where
        unlockedNote = fromJust $ find (not . _locked) (st ^. notes)
        focus = focusGetCurrent (unlockedNote ^. focusRing)
        updateEditor ed note | (not . _locked) note = note & focusedField note . Field.editor .~ ed
        updateEditor ed note = note

-- Editor has C-e, C-a, C-d, C-k, C-u, arrows, enter, paste. Should probably
-- keep away from those
eventHandler :: St -> BrickEvent Resource e -> EventM Resource (Next St)
eventHandler st (VtyEvent ev)  =
     case ev of
        EvKey (KChar '\t') [] -> continue (st & notes %~ activate succ)
        EvKey KBackTab [] -> continue (st & notes %~ activate pred)
        -- EvKey (KChar '\t') [] | isEditing st -> continue (st & notes %~ activate succ)
        EvKey (KChar 'o') [MCtrl] -> continue (st & notes %~ unlockActive)
        EvKey (KChar 's') [MCtrl] -> continue (st & notes %~ lockActive)
        EvKey (KChar 'g') [MCtrl] | isEditing st -> continue (st & notes %~ lockActive)
        EvKey (KChar 'g') [MCtrl] -> halt st
        EvKey (KChar 'c') [MCtrl] -> halt st
        _ | isEditing st -> continue =<< handleEventLensed st updateUnlockedEditor handleEditorEvent ev
        _ -> continue st
eventHandler st _ = continue st

