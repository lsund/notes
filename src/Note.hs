{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Note where

import           Data.Text                  (unlines)
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.TH
import           Prelude                    hiding (id, unlines)

import           Brick.AttrMap              (AttrName, applyAttrMappings)
import           Brick.Focus                (FocusRing, focusGetCurrent, withFocusRing)
import           Brick.Types                (Widget)
import           Brick.Util                 (fg, on)
import           Brick.Widgets.Border       (borderAttr, borderWithLabel, hBorderWithLabel)
import           Brick.Widgets.Border.Style (ascii)
import           Brick.Widgets.Center       (center)
import           Brick.Widgets.Core         (hBox, hLimit, txt, updateAttrMap, vLimit, withAttr, withBorderStyle, (<=>))
import           Brick.Widgets.Edit         (renderEditor)
import           Graphics.Vty               (Attr, black, blue, cyan, yellow)

import           Field                      (Field (..))
import qualified Field
import           Resource

data Note = Note
              { _id        :: Integer
              , _active    :: Bool
              , _locked    :: Bool
              , _title     :: Field
              , _content   :: Field
              , _focusRing :: FocusRing Resource
              }
  deriving (Generic)

makeLenses ''Note

instance Show Note where
    show (Note i a l t c f) =
        "Note[" <> show  i <> "](active: " <> show  a <> ", locked: "  <> show l  <> ", title: " <>  show t  <> ", content: " <> show c <> ", focused: " <> show (focusGetCurrent f) <> ")"

instance Eq Note where
    note1 == note2 = _id note1 == _id note2

instance Ord Note where
    note1 `compare` note2 = _id note1 `compare` _id note2


borderMappings :: Bool -> [(AttrName, Attr)]
borderMappings active =
    [ (borderAttr, (if active then yellow else blue) `on` black)
    , ("title", fg cyan)
    ]

render :: Note -> Widget Resource -> Widget Resource
render note content =
    updateAttrMap (applyAttrMappings (borderMappings (note ^. active))) $
    withBorderStyle ascii $
    borderWithLabel (withAttr "title" $ txt (note ^. (title . Field.content))) content

renderUnlocked :: Note -> Widget Resource
renderUnlocked note =
    let titleEditor = withFocusRing (note ^. focusRing) (renderEditor (txt . unlines)) (note ^. (title . Field.editor))
        contentEditor = withFocusRing (note ^. focusRing) (renderEditor (txt . unlines)) (note ^. (content . Field.editor))
     in
       hLimit 30 (vLimit 1 titleEditor) <=> hLimit 30 (vLimit 5 contentEditor)

renderLocked :: Note -> Widget Resource
renderLocked note =
    hLimit 20 $
    vLimit 5 $
    center $ txt (note ^. (content . Field.content))

renderMany :: [Note] -> Widget Resource
renderMany notes =
    hBorderWithLabel (txt "Existing notes")
    <=> hBox (map
                (\note -> if _locked note then render note (renderLocked note)  else render note (renderUnlocked note))
                notes)


