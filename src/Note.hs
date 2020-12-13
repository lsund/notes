{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Note where

import Shared (Name, Name(..))
import Lens.Micro
import Lens.Micro.TH
import Brick.Types (Widget)
import Data.Text (Text)
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Focus as F
import qualified Graphics.Vty as V
import Brick.Util (fg, on)
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , str
  )

data Note = Note { _title :: String, _content :: String }

makeLenses ''Note

instance Show Note where
    show (Note title content) = title <> "#" <> content

styles :: [(Text, BS.BorderStyle)]
styles =
    [ ("ascii", BS.ascii)
    , ("unicode", BS.unicode)
    , ("unicode bold", BS.unicodeBold)
    , ("unicode rounded", BS.unicodeRounded)
    , ("from 'x'", BS.borderStyleFromChar 'x')
    ]

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr, V.blue `on` V.black)
    , ("title", fg V.cyan)
    ]

renderEditable :: F.FocusRing Name -> E.Editor String Name -> E.Editor String Name -> Widget Name
renderEditable focusRing titleEditor contentEditor =
    let title = F.withFocusRing focusRing (E.renderEditor (str . unlines)) titleEditor
        content = F.withFocusRing focusRing (E.renderEditor (str . unlines)) contentEditor
     in
        B.hBorderWithLabel (str "Hit 'Esc' to save and 'Tab' to switch focus")
        <=> C.center (str "Title   " <+> hLimit 30 (vLimit 5 title) <=>
                      (str "Content " <+> hLimit 30 (vLimit 5 content)))

render :: Note -> Widget Name
render x =
    updateAttrMap (A.applyAttrMappings borderMappings) $
    withBorderStyle BS.ascii $
    B.borderWithLabel (withAttr "title" $ str (x^.title)) $
    hLimit 20 $
    vLimit 5 $
    C.center $
    str (x^.content)

renderMany :: [Note] -> Widget Name
renderMany xs =
    B.hBorderWithLabel (str "Existing notes")
    <=> hBox (map render xs)
    <=> B.hBorderWithLabel (str "New note")
    <=> C.center (str "Hit 'Esc' to create a new note, or 'q' to save and quit")
