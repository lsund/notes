module Editor where

import           Brick.Focus        (focusGetCurrent)
import           Brick.Widgets.Edit (Editor, editContentsL)
import           Data.Char          (isSpace)
import           Data.List
import           Data.List.HT       (takeUntil)
import           Data.Text          (Text, pack)
import           Data.Text.Zipper
import           Data.Time.Clock
import           Lens.Micro
import           Prelude            hiding (Left, Right)
import           Safe               (lastMay, tailMay)

import           Field              (Field (..), editor)
import           Note               (Note (..), active, content, focusRing, locked, title, updated)
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

atLineLimit :: Int ->TextZipper a -> Bool
atLineLimit lim = (== lim) . snd . cursorPosition

atLineBegin :: TextZipper a -> Bool
atLineBegin = atLineLimit 0

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

scanWordTo :: St -> Direction -> Maybe String
scanWordTo st dir = st ^. notes . to (find (^. active)) >>= scanWordTo'
    where
        scanWordTo' note =
                let editor = (note ^. Note.content . Field.editor . editContentsL)
                    (moveFn, limit) = if dir == Left then (moveLeft, 0) else (moveRight, noteWidth)
                    scan = sequence . takeWhile (maybe False (not . isSpace)) . map currentChar . takeUntil (atLineLimit limit)  . iterate moveFn
                 in scan editor

scanWord :: St -> Maybe Text
scanWord st = strip . pack <$> (reverse <$> scanWordTo st Left) <> (scanWordTo st Right >>= tailMay)

killWord :: [Note] -> [Note]
killWord = map (\note -> if note ^. active
                            then let fieldFn = case note ^. focusRing . to focusGetCurrent of
                                                Just (Resource _ Title) -> Note.title
                                                _                       -> Note.content
                                  in note & fieldFn . Field.editor . editContentsL %~ deleteFn
                            else note)
    where
          deleteWhitespace = lastMay . (takeUntil (not . maybe False isSpace . currentChar) . iterate (moveLeft . deleteChar))
          deleteWord = lastMay . (takeUntil (\editor -> (maybe False isSpace . currentChar) editor || atLineBegin editor) . iterate (moveLeft . deleteChar))
          deleteAt0 editor | atLineBegin editor = deleteChar editor
          deleteAt0 editor = editor
          deleteFn editor = maybe editor deleteAt0 $ deleteWhitespace editor >>= deleteWord


nudgeCursor :: (TextZipper Text -> TextZipper Text) -> [Note] -> [Note]
nudgeCursor moveFn = map (\note -> if note ^. active then note & Note.content . Field.editor . editContentsL %~ moveFn else note)

