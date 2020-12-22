{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Event where

import           Control.Monad.IO.Class (liftIO)
import           Data.List              hiding (unlines)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text, unlines)
import           Data.Time.Clock
import           Lens.Micro
import           Prelude                hiding (unlines)

import           Brick.Focus            (focusGetCurrent, focusNext)
import           Brick.Main             (continue, halt)
import           Brick.Types            (BrickEvent (..), EventM, Next, handleEventLensed)
import           Brick.Widgets.Edit     (Editor, editorText, getEditContents, handleEditorEvent)
import           Graphics.Vty           (Event (EvKey), Key (KBackTab, KChar, KEnter), Modifier (MCtrl))

import           Field                  (Field (..))
import qualified Field
import           Note                   (Note (..), focusRing, locked)
import qualified Note
import           Prim
import           Resource
import           State

activateOnId :: Integer -> [Note] -> [Note]
activateOnId id  = map (\note -> if _id note == id then note { _active = True } else note { _active = False })

-- There should be exactly one active note at all times. Otherwise, something
-- is wrong with this function
activate :: (Integer -> Integer) -> [Note] -> [Note]
activate next xs =
    let maxIndex = maximum $ map _id xs
     in case find _active xs of
      Nothing         -> error "Should not happen"
      Just activeNote -> activateOnId (nextIndex (_id activeNote) maxIndex) xs
    where
        nextIndex aid mid | next aid > mid = 0
        nextIndex aid mid | next aid < 0 = mid
        nextIndex aid _   = next aid


focusedResource :: St -> Maybe Resource
focusedResource st =
    let activeNote = fromJust $ find _active (st ^. notes)
     in focusedResource' activeNote

focusedResource' :: Note -> Maybe Resource
focusedResource' note = focusGetCurrent (note ^. focusRing)

focusedField :: Functor f => Note -> (Field -> f Field) -> Note -> f Note
focusedField note = case focusedResource' note of
                Just (Resource _ Title) -> Note.title
                _                       -> Note.content

isEditing :: St -> Bool
isEditing st = (not . null) (filter (\n -> _active n && (not . _locked) n) (st ^. notes))

isEditingTitle :: St -> Bool
isEditingTitle st = isEditing st && case focusedResource st of Just (Resource _ Title) -> True; _ -> False

unlockActive :: [Note] -> [Note]
unlockActive = map (\note@(Note i a l (Field t _) (Field c _) foc _) ->
                        if a
                              then let tt = editorText (Resource i Title) Nothing t
                                       tc = editorText (Resource i Content) Nothing c
                                    in note & Note.title . Field.editor .~ tt & Note.content . Field.editor .~ tc & locked .~ False
                        else note)

lockActive :: [Note] -> [Note]
lockActive =
    map (\note@(Note i a l (Field t te) (Field c ce) _ _) ->
        if a
           then let ce' = unlines (getEditContents ce)
                    te' = unlines (getEditContents te)
                 in note & Note.title . Field.content .~ te' & Note.content . Field.content .~ ce' & locked .~ True
            else note)

updateUnlockedEditor :: Functor f => (Editor Text Resource -> f (Editor Text Resource)) -> St -> f St
updateUnlockedEditor f st =
    let  focusedEditor = case focusedResource st of
                            Just (Resource _ Title) -> Field._editor (Note._title activeNote)
                            _                       -> Field._editor (Note._content activeNote)
     in (\editor' -> st & notes %~ map (updateEditor editor')) <$> f focusedEditor
     where
        activeNote = fromJust $ find _active (st ^. notes)
        updateEditor ed note | _active note && (not . _locked) note = note & focusedField note . Field.editor .~ ed
        updateEditor ed note = note

toggleFocus :: [Note] -> [Note]
toggleFocus =
    map (\note@(Note i a l (Field t te) (Field c ce) foc _) ->
        if a
           then note & Note.focusRing %~ focusNext
            else note)

updateTime :: UTCTime -> [Note] -> [Note]
updateTime ct = map (\note@(Note i a l t c foc ut) -> if a then Note i a l t c foc ct else note)

-- Editor has C-e, C-a, C-d, C-k, C-u, arrows, enter, paste. Should probably
-- keep away from those
--
-- Tmux Blocks meta/alt
eventHandler :: St -> BrickEvent Resource e -> EventM Resource (Next St)
eventHandler st (VtyEvent ev)  =
    let editing = isEditing st
        editingTitle = isEditingTitle st
    in case ev of
        -- Ignoring
        EvKey KEnter [] | editingTitle -> continue st
        -- Toggling focus
        EvKey (KChar '\t') [] | editing -> continue (st & notes %~ toggleFocus)
        -- Toggling active
        EvKey (KChar '\t') [] -> continue (st & notes %~ activate succ)
        EvKey KBackTab [] -> continue (st & notes %~ activate pred)
        EvKey (KChar 'l') [] | not editing -> continue (st & notes %~ activate succ)
        EvKey (KChar 'h') [] | not editing -> continue (st & notes %~ activate pred)
        -- Lock/unlock
        EvKey (KChar 'g') [MCtrl] | editing -> continue (st & notes %~ lockActive)
        EvKey (KChar 'o') [MCtrl] -> continue (st & notes %~ unlockActive)
        EvKey (KChar 's') [MCtrl] -> continue (st & notes %~ lockActive)
        -- Stop
        EvKey (KChar 'g') [MCtrl] -> halt st
        EvKey (KChar 'c') [MCtrl] -> halt st
        -- Editor event
        _ | editing -> do
            ct <- liftIO getCurrentTime
            let st' = st & notes %~ updateTime ct
            continue =<< handleEventLensed st' updateUnlockedEditor handleEditorEvent ev
        _ -> continue st
eventHandler st _ = continue st

