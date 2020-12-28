{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Event where

import           Control.Monad.IO.Class (liftIO)
import           Data.List              hiding (unlines)
import           Data.List.HT           (takeUntil)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text, pack, unlines)
import           Data.Text.Zipper
import           Data.Time.Clock
import           Lens.Micro
import           Prelude                hiding (unlines)
import           Safe                   (tailMay, maximumMay)

import           Brick.Focus            (focusGetCurrent, focusNext)
import qualified Brick.Focus            as Focus
import           Brick.Main             (continue, halt)
import           Brick.Types            (BrickEvent (..), EventM, Next, handleEventLensed)
import           Brick.Widgets.Edit     (Editor, editContentsL, editorText, getEditContents, handleEditorEvent)
import           Graphics.Vty           (Event (EvKey), Key (KBackTab, KChar, KEnter), Modifier (MCtrl))

import           Field                  (Field (..), editor)
import qualified Field
import           Note                   (Note (..), active, content, focusRing, locked, title, updated)
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

maxId :: [Note] -> Int
maxId = fromMaybe 0 . maximumMay . map (^. Note.id)

-------------------------------------------------------------------------------
-- Toggling Active

activateOnId :: Int -> [Note] -> [Note]
activateOnId id  = map (\note -> if note ^. Note.id == id then note & active .~ True else note & active .~ False & locked .~ True)

activateLast :: [Note] -> [Note]
activateLast xs = activateOnId (maxId xs) xs

-- There should be exactly one active note at all times. Otherwise, something
-- is wrong with this function
activate :: (Int -> Int) -> [Note] -> [Note]
activate next xs =
    let index = case find (^. active) xs of
                    Nothing         -> 0
                    Just activeNote -> nextIndex (activeNote ^. Note.id) (maxId xs)
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

addNote :: Maybe Text -> UTCTime -> [Note] -> [Note]
addNote title ct notes =
    let i = maybe 0 succ (maximumMay (map (^. Note.id) notes))
        title' = fromMaybe "" title
        note = Note
                i
                (null notes)
                True
                (Field title' (editorText (Resource i Title) Nothing title'))
                (Field "" (editorText (Resource i Content) Nothing ""))
                (Focus.focusRing [Resource i Content, Resource i Title])
                ct
                ct
     in sort $ note : notes

deleteNote :: Int -> [Note] -> [Note]
deleteNote id xs =
    let oneLess = foldr (\note acc -> if note ^. active then acc else note : acc) [] xs
        maxIndex = fromIntegral $ pred $ length oneLess
        (_, reIndexed) = foldr
                        (\note@(Note _ a l (Field t te) (Field c ce) foc ct ut) (i, acc) ->
                            let title' = Field t (editorText (Resource i Title) Nothing t)
                                content' = Field c (editorText (Resource i Content) Nothing c)
                             in (pred i, Note i a l title' content' foc ct ut :  acc))
                        (maxIndex, [])
                        oneLess
                             in reIndexed & ix (max (pred id) 0) . Note.active .~ True

-------------------------------------------------------------------------------
    -- Editor Event

updateTime :: UTCTime -> [Note] -> [Note]
updateTime currentTime = map (\note -> if note ^. active then note & updated .~ currentTime else note)

handleCharacter :: Functor f => Note -> (Editor Text Resource -> f (Editor Text Resource)) -> St -> f St
handleCharacter note editor st =
    let focusedEditor = case focusedResource st of
                            Just (Resource _ Title) -> Field._editor (Note._title note)
                            _                       -> Field._editor (Note._content note)
     in (\editor' -> st & notes %~ map (updateEditor editor')) <$> editor focusedEditor
     where
        updateEditor ed note | note ^. active && not (note ^. locked) = note & focusedField note . Field.editor .~ ed
        updateEditor ed note = note

scanWord :: St -> (TextZipper Text -> TextZipper Text) -> Maybe String
scanWord st moveFn = st ^. notes . to (find (^. active)) >>= scanWord'
    where scanWord' note =
                let editContents = (note ^. Note.content . Field.editor . editContentsL)
                    scan = sequence . takeWhile notSpace . map currentChar . takeUntil (not . changed) . iterate moveFn
                    -- scan = sequence . takeWhile notSpace . map currentChar . takeUntil ((== 0) . fst . cursorPosition) . iterate moveFn
                 in scan editContents
          changed zipper = cursorPosition (moveFn zipper) /= cursorPosition zipper
          notSpace Nothing    = False
          notSpace (Just ' ') = False
          notSpace _          = True

-- TODO
-- killWord = map (\note -> if note ^. active then note & Note.content . Field.editor . editContentsL %~ scan else note)
--     where scan editor = last $ (takeUntil foo . iterate deletePrevChar) editor
--           foo x = (not . changed) x || isSpace x
--           changed zipper = cursorPosition (moveLeft zipper) /= cursorPosition zipper
--           isSpace x | currentChar x == (Just ' ') = True
--           isSpace _          = False


nudgeCursor :: (TextZipper Text -> TextZipper Text) -> [Note] -> [Note]
nudgeCursor moveFn = map (\note -> if note ^. active then note & Note.content . Field.editor . editContentsL %~ moveFn else note)


-------------------------------------------------------------------------------
-- Main
--
-- Editor has C-e, C-a, C-d, C-k, C-u, arrows, enter, paste. Should probably
-- keep away from those
--
-- Tmux Blocks meta/alt
eventHandler :: St -> BrickEvent Resource e -> EventM Resource (Next St)
eventHandler st (VtyEvent ev)  =
    case find (^. active) (st ^. notes) of
        Nothing         -> continue st
        Just activeNote -> do
            ct <- liftIO getCurrentTime
            let editing = isEditing st
                editingTitle = isEditingTitle st
                activeId = activeNote ^. Note.id
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
                EvKey (KChar 'n') [MCtrl] | not editing -> continue (st & notes %~ addNote Nothing ct)
                EvKey (KChar 'd') [MCtrl] | not editing -> continue (st & notes %~ deleteNote activeId)
                -- Follow link
                EvKey (KChar 'l') [MCtrl] | editing     ->
                    case (reverse <$> scanWord st moveLeft) <> (scanWord st moveRight >>= tailMay) of
                        Just word ->
                            case find (\note -> note ^. title . Field.content == pack word) (st ^. notes) of
                                Just note -> continue (st & notes %~ activateOnId (note ^. Note.id))
                                Nothing   -> continue ((st & notes %~ addNote ((Just . pack) word) ct) & notes %~ activateLast)
                        Nothing -> continue st
                -- Stop
                EvKey (KChar 'g') [MCtrl]               -> halt st
                EvKey (KChar 'c') [MCtrl]               -> halt st
                -- Movement while editing
                -- Editor event
                -- EvKey (KChar 'w') [MCtrl] | editing     -> continue (st & notes %~ killWord)
                EvKey (KChar 'b') [MCtrl] | editing     -> continue (st & notes %~ nudgeCursor moveLeft)
                EvKey (KChar 'f') [MCtrl] | editing     -> continue (st & notes %~ nudgeCursor moveRight)
                EvKey (KChar 'n') [MCtrl] | editing     -> continue (st & notes %~ nudgeCursor moveDown)
                EvKey (KChar 'p') [MCtrl] | editing     -> continue (st & notes %~ nudgeCursor moveUp)
                _ | editing                             ->
                    case find (^. active) (st ^. notes) of
                        Just activeNote -> continue =<< handleEventLensed (st & notes %~ updateTime ct) (handleCharacter activeNote) handleEditorEvent ev
                        Nothing         -> continue st
                _ -> continue =<< handleEventLensed (st & notes %~ updateTime ct) (handleCharacter activeNote) handleEditorEvent ev
eventHandler st _ = continue st

