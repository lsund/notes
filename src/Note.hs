{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Note where

import           Data.Text                  (pack, unlines)
import           Data.Time.Clock
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.TH
import           Prelude                    hiding (id, unlines)

import           Brick.AttrMap              (applyAttrMappings)
import           Brick.Focus                (FocusRing, focusGetCurrent, withFocusRing)
import           Brick.Markup               (markup, (@?))
import           Brick.Types                (Padding (..), Widget)
import           Brick.Util                 (on)
import           Brick.Widgets.Border       (borderAttr, borderWithLabel, hBorderWithLabel)
import           Brick.Widgets.Border.Style (ascii)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core         (hBox, hLimit, padTop, txt, updateAttrMap, vLimit, withAttr,
                                             withBorderStyle, (<=>))
import           Brick.Widgets.Edit         (renderEditor)
import           Graphics.Vty               (black, blue, yellow)

import           Field                      (Field (..))
import qualified Field
import           Prim
import           Resource

data Note = Note
              { _id          :: Integer
              , _active      :: Bool
              , _locked      :: Bool
              , _title       :: Field
              , _content     :: Field
              , _focusRing   :: FocusRing Resource
              , _lastUpdated :: UTCTime
              }
  deriving (Generic)

makeLenses ''Note

instance Show Note where
    show (Note i a l t c f ut) =
        "Note[" <> show  i <> "](active: " <> show  a
        <> ", locked: "  <> show l
        <> ", last update time: " <> showLastUpdated ut
        <> ", title: " <>  show t
        <> ", content: " <> show c
        <> ", focused: " <> show (focusGetCurrent f) <> ")"

instance Eq Note where
    note1 == note2 = _id note1 == _id note2

instance Ord Note where
    note1 `compare` note2 = _id note1 `compare` _id note2

height :: Int
height = 10

width :: Int
width = 30

showLastUpdated :: UTCTime -> String
showLastUpdated = formatTime defaultTimeLocale formatStr


renderMetadata :: Note -> Widget Resource
renderMetadata note = padTop (Pad 2) $ shaded (pack (showLastUpdated (note ^. lastUpdated)))
    where shaded c = markup (c @? "meta")

render :: Note -> Widget Resource
render note =
    updateAttrMap (applyAttrMappings (borderStyle (note ^. active))) $
    withBorderStyle ascii $
    borderWithLabel (withAttr "title" $ txt (note ^. (title . Field.content))) $
        hLimit width $
        vLimit height $
        center $
            txt (note ^. (content . Field.content))
            <=> renderMetadata note
    where
        borderStyle active = [ (borderAttr, (if active then yellow else blue) `on` black) ]

renderUnlocked :: Note -> Widget Resource
renderUnlocked note =
    let titleEditor = withFocusRing (note ^. focusRing) (renderEditor (txt . unlines)) (note ^. (title . Field.editor))
        contentEditor = withFocusRing (note ^. focusRing) (renderEditor (txt . unlines)) (note ^. (content . Field.editor))
     in
       hLimit width (vLimit 1 titleEditor) <=> hLimit width (vLimit height contentEditor)

renderMany :: [Note] -> Widget Resource
renderMany notes =
    hBorderWithLabel (txt "Existing notes")
    <=> hBox (map
                (\note -> if _locked note then render note  else renderUnlocked note)
                notes)


