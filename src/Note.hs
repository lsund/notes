{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Note where

import           Data.Text                  (Text, lines, unlines, unpack, words, tail)
import           Data.Time.Clock
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.TH
import           Prelude                    hiding (id, lines, unlines, words, tail)
import           Safe                       (headMay)

import           Brick.AttrMap              (applyAttrMappings)
import           Brick.Focus                (FocusRing, focusGetCurrent, withFocusRing)
import           Brick.Markup               (markup, (@?))
import           Brick.Types                (Padding (..), Widget)
import           Brick.Util                 (on)
import           Brick.Widgets.Border       (borderAttr, borderWithLabel)
import           Brick.Widgets.Border.Style (ascii)
import           Brick.Widgets.Core         (hBox, hLimit, padBottom, padLeft, padRight, padTop, txt, updateAttrMap,
                                             vBox, vLimit, withAttr, withBorderStyle, (<=>))
import           Brick.Widgets.Edit         (renderEditor)
import           Graphics.Vty               (black, blue, yellow)

import           Field                      (Field (..))
import qualified Field
import           Prim
import           Resource

data Note = Note
              { _id        :: Int
              , _active    :: Bool
              , _locked    :: Bool
              , _title     :: Field
              , _content   :: Field
              , _focusRing :: FocusRing Resource
              , _created   :: UTCTime
              , _updated   :: UTCTime
              }
  deriving (Generic)

makeLenses ''Note

instance Show Note where
    show (Note i a l t c f ct ut) =
        "Note[" <> show  i <> "](active: " <> show  a
        <> ", locked: "  <> show l
        <> ", Created time: " <> (unpack . unparseTime) ct
        <> ", last update time: " <> (unpack . unparseTime) ut
        <> ", title: " <>  show t
        <> ", content: " <> show c
        <> ", focused: " <> show (focusGetCurrent f) <> ")"

instance Eq Note where
    note1 == note2 = _id note1 == _id note2

instance Ord Note where
    note1 `compare` note2 = _id note1 `compare` _id note2

renderMetadata :: Note -> Widget Resource
renderMetadata note = padTop (Pad 2) $ shaded (unparseTime (note ^. updated))
    where shaded c = markup (c @? "meta")

renderContent :: [Text] -> Text -> Widget Resource
renderContent titles text | strip text ==  "" = txt "<empty>"
renderContent titles text = (vBox . map renderLine . lines) text
    where
        renderLine = hBox . map renderWord . words
        renderWord x | x `elem` titles = (withAttr "link" . txt . (<> " ")) x
        renderWord x | (headMay . unpack) x == Just '#' = (withAttr "heading1" . txt . (<> "\n ") . tail) x
        renderWord x = (txt . (<> " ")) x

render :: [Text] -> Note -> Widget Resource
render titles note =
    updateAttrMap (applyAttrMappings (borderStyle a)) $
    withBorderStyle ascii $
    borderWithLabel (withAttr "title" $ txt (note ^. (title . Field.content))) $
        hLimit (noteWidth a) $
        vLimit (noteHeight a) $
          padTop (Pad 2) $
          padBottom (Pad 2) $
          padLeft (Pad 1) $
          padRight Max $
            renderContent titles (note ^. (content . Field.content))
              where
                a = note ^. active
                borderStyle active = [ (borderAttr, (if active then yellow else blue) `on` black) ]

renderUnlocked :: Note -> Widget Resource
renderUnlocked note =
    let titleEditor = withFocusRing (note ^. focusRing) (renderEditor (txt . unlines)) (note ^. (title . Field.editor))
        contentEditor = withFocusRing (note ^. focusRing) (renderEditor (txt . unlines)) (note ^. (content . Field.editor))
        a = note ^. active
     in
       hLimit (noteWidth a) (vLimit 1 titleEditor) <=> hLimit (noteWidth a) (vLimit (noteHeight a) contentEditor)

renderMany :: [Note] -> Widget Resource
renderMany notes =
    hBox (map
            (\note -> if _locked note then render (map (^. title . Field.content) notes) note else renderUnlocked note)
            notes)


