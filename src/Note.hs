{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Note where

import GHC.Generics
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
import Data.Aeson (FromJSON, ToJSON)

data Note = Note { _id :: Integer, _active :: Bool,  _title :: String, _content :: [String] }
    deriving (Generic, Show)

instance FromJSON Note

instance ToJSON Note

instance Eq Note where
    note1 == note2 = _id note1 == _id note2

instance Ord Note where
    note1 `compare` note2 = _id note1 `compare` _id note2


makeLenses ''Note

styles :: [(Text, BS.BorderStyle)]
styles =
    [ ("ascii", BS.ascii)
    , ("unicode", BS.unicode)
    , ("unicode bold", BS.unicodeBold)
    , ("unicode rounded", BS.unicodeRounded)
    , ("from 'x'", BS.borderStyleFromChar 'x')
    ]

borderMappings :: Bool -> [(A.AttrName, V.Attr)]
borderMappings active =
    [ (B.borderAttr, (if active then V.yellow else V.blue) `on` V.black)
    , ("title", fg V.cyan)
    ]

renderEditable :: F.FocusRing Name -> E.Editor String Name -> E.Editor String Name -> Widget Name
renderEditable focusRing titleEditor contentEditor =
    let title = F.withFocusRing focusRing (E.renderEditor (str . unlines)) titleEditor
        content = F.withFocusRing focusRing (E.renderEditor (str . unlines)) contentEditor
     in
        B.hBorderWithLabel (str "Hit 'C-s' to save and 'Tab' to switch focus")
        <=> C.center (str "Title   " <+> hLimit 30 (vLimit 5 title) <=>
                      (str "Content " <+> hLimit 30 (vLimit 5 content)))

render :: Note -> Widget Name
render note =
    updateAttrMap (A.applyAttrMappings (borderMappings (note^.active))) $
    withBorderStyle BS.ascii $
    B.borderWithLabel (withAttr "title" $ str (note^.title)) $
    hLimit 20 $
    vLimit 5 $
    C.center $
    str (foldr (\note acc -> "* " <> note <> "\n" <> acc) "" (note^.content))

renderMany :: [Note] -> Widget Name
renderMany xs =
    B.hBorderWithLabel (str "Existing notes")
    <=> hBox (map render xs)
    <=> B.hBorderWithLabel (str "New note")
    <=> C.center (str "Hit 'C-n' to create a new note, or 'C-g' to save and quit")
