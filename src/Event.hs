{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Event where

import           Control.Monad.IO.Class (liftIO)
import           Data.List              hiding (unlines)
import           Data.Maybe             (isJust)
import           Data.Text              (Text, unlines)
import           Data.Text.Zipper
import           Data.Time.Clock
import           Lens.Micro
import           Prelude                hiding (unlines)

import           Brick.Focus            (focusGetCurrent, focusNext)
import qualified Brick.Focus            as Focus
import           Brick.Main             (continue, halt)
import           Brick.Types            (BrickEvent (..), EventM, Next, handleEventLensed)
import           Brick.Widgets.Edit     (Editor, editContentsL, editorText, getEditContents, handleEditorEvent)
import           Graphics.Vty           (Event (EvKey), Key (KBackTab, KChar, KEnter), Modifier (MCtrl))

import           Field                  (Field (..), editor)
import qualified Field
import           Note                   (Note (..), focusRing, locked, active, updated, title, content)
import qualified Note
import           Prim
import           Resource
import           State

focusedResource :: St -> Maybe Resource
focusedResource st = find (^. active) (st ^. notes) >>= focusedResource'

focusedResource' :: Note -> Maybe Resource
focusedResource' note = focusGetCurrent (note ^. focusRing)

focusedField :: Functor f => Note -> (Field -> f Field) -> Note -> f Note
focusedField note = case focusedResource' note of
                Just (Resource _ Title) -> title
                _                       -> content

isEditing :: St -> Bool
isEditing st = (not . null) (filter (\note -> note ^. active && not (note ^. locked)) (st ^. notes))

isEditingTitle :: St -> Bool
isEditingTitle st = isEditing st && case focusedResource st of Just (Resource _ Title) -> True; _ -> False

-------------------------------------------------------------------------------
-- Toggling Active

activateOnId :: Int -> [Note] -> [Note]
activateOnId id  = map (\note -> if note ^. Note.id == id then note & active .~ True else note & active .~ False)

-- There should be exactly one active note at all times. Otherwise, something
-- is wrong with this function
activate :: (Int -> Int) -> [Note] -> [Note]
activate next xs =
    let maxIndex = maximum $ map _id xs
        index = case find (^. active) xs of
                    Nothing         -> 0
                    Just activeNote -> nextIndex (activeNote ^. Note.id) maxIndex
    in activateOnId index xs
    where
        nextIndex aid mid | next aid > mid = 0
        nextIndex aid mid | next aid < 0 = mid
        nextIndex aid _   = next aid

unlockActive :: [Note] -> [Note]
unlockActive = map (\note ->
                        if note ^. active
                           then let i = (note ^. Note.id)
                                    tt = editorText (Resource i Title) Nothing (note ^. title . Field.content)
                                    tc = editorText (Resource i Content) Nothing (note ^. content . Field.content)
                                in note & title . Field.editor .~ tt & content . Field.editor .~ tc & locked .~ False
                        else note)

lockActive :: [Note] -> [Note]
lockActive =
    map (\note ->
        if note ^. active
           then let ce' = unlines (getEditContents (note ^. content . Field.editor))
                    te' = unlines (getEditContents (note ^. title . Field.editor))
                 in note & Note.title . Field.content .~ te' & content . Field.content .~ ce' & locked .~ True
            else note)

-------------------------------------------------------------------------------
-- Toggling Focus

toggleFocus :: [Note] -> [Note]
toggleFocus = map (\note -> if note ^. active then note & Note.focusRing %~ focusNext else note)

-------------------------------------------------------------------------------
    -- Create / Delete

addNote :: UTCTime -> [Note] -> [Note]
addNote ct xs =
    let i = succ $ maximum $ map _id xs
        x = Note
                i
                False
                True
                (Field "" (editorText (Resource i Title) Nothing ""))
                (Field "" (editorText (Resource i Content) Nothing ""))
                (Focus.focusRing [Resource i Title, Resource i Content])
                ct
                ct
     in sort $ x : xs

deleteNote :: [Note] -> [Note]
deleteNote xs =
    let oneLess = foldr (\note acc -> if note ^. active then acc else note : acc) [] xs
        maxIndex = fromIntegral $ pred $ length oneLess
        (_, reIndexed) = foldr
                        (\note@(Note _ a l (Field t te) (Field c ce) foc ct ut) (i, acc) ->
                            let title' = Field t (editorText (Resource i Title) Nothing t)
                                content' = Field c (editorText (Resource i Content) Nothing c)
                             in (pred i, Note i a l title' content' foc ct ut :  acc))
                        (maxIndex, [])
                        oneLess
                             in reIndexed & ix 0 . Note.active .~ True

-------------------------------------------------------------------------------
    -- Editor Event

updateTime :: UTCTime -> [Note] -> [Note]
updateTime currentTime = map (\note -> if note ^. active then note & updated .~ currentTime else note)

updateEditorFor :: Functor f => Note -> (Editor Text Resource -> f (Editor Text Resource)) -> St -> f St
updateEditorFor note editor st =
    let focusedEditor = case focusedResource st of
                            Just (Resource _ Title) -> Field._editor (Note._title note)
                            _                       -> Field._editor (Note._content note)
     in (\editor' -> st & notes %~ map (updateEditor editor')) <$> editor focusedEditor
     where
        updateEditor ed note | note ^. active && not (note ^. locked) = note & focusedField note . Field.editor .~ ed
        updateEditor ed note = note


-- TODO we can get the full word here
-- This function returns everything to the right of the cursor
showRight st =
    case st ^. notes . to (find (^. active)) of
      Nothing -> Just "Error"
      Just x  -> (sequence . takeWhile isJust . map currentChar . iterate moveRight) (x ^. Note.content . Field.editor . editContentsL)

-------------------------------------------------------------------------------
-- Main
--
-- Editor has C-e, C-a, C-d, C-k, C-u, arrows, enter, paste. Should probably
-- keep away from those
--
-- Tmux Blocks meta/alt
eventHandler :: St -> BrickEvent Resource e -> EventM Resource (Next St)
eventHandler st (VtyEvent ev)  = do
    ct <- liftIO getCurrentTime
    let editing = isEditing st
        editingTitle = isEditingTitle st
    case ev of
      -- Ignoring
        EvKey KEnter [] | editingTitle          -> continue st
        -- Toggling focus
        EvKey (KChar '\t') [] | editing         -> continue (st & notes %~ toggleFocus)
        -- Toggling active
        EvKey (KChar '\t') []                   -> continue (st & notes %~ activate succ)
        EvKey KBackTab []                       -> continue (st & notes %~ activate pred)
        EvKey (KChar 'l') [] | not editing      -> continue (st & notes %~ activate succ)
        EvKey (KChar 'h') [] | not editing      -> continue (st & notes %~ activate pred)
        -- Lock/unlock
        EvKey (KChar 'g') [MCtrl] | editing     -> continue (st & notes %~ lockActive)
        EvKey (KChar 'o') [MCtrl]               -> continue (st & notes %~ unlockActive)
        EvKey (KChar 's') [MCtrl]               -> continue (st & notes %~ lockActive)
        -- Create / Delete
        EvKey (KChar 'n') [MCtrl] | not editing -> continue (st & notes %~ addNote ct)
        EvKey (KChar 'd') [MCtrl] | not editing -> continue (st & notes %~ deleteNote)
        -- Follow link
        EvKey (KChar 'l') [MCtrl] | editing     -> do
            liftIO $ print $ showRight st
            continue st
        -- Stop
        EvKey (KChar 'g') [MCtrl]               -> halt st
        EvKey (KChar 'c') [MCtrl]               -> halt st
        -- Editor event
        _ | editing ->
            case find (^. active) (st ^. notes) of
                Just activeNote -> continue =<< handleEventLensed (st & notes %~ updateTime ct) (updateEditorFor activeNote) handleEditorEvent ev
                Nothing         -> continue st
        _ -> continue st
eventHandler st _ = continue st

