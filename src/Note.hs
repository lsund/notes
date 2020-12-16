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
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , str
  )
import Data.Aeson (FromJSON, ToJSON)

data Note = Note { _id :: Integer, _active :: Bool, _locked :: Bool,  _title :: String, _content :: [String] }
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

render :: Note -> Widget Name -> Widget Name
render note content =
    updateAttrMap (A.applyAttrMappings (borderMappings (note^.active))) $
    withBorderStyle BS.ascii $
    B.borderWithLabel (withAttr "title" $ str (note^.title)) content

renderUnlocked :: F.FocusRing Name -> E.Editor String Name -> E.Editor String Name -> Widget Name
renderUnlocked focusRing titleEditor contentEditor =
    let title = F.withFocusRing focusRing (E.renderEditor (str . unlines)) titleEditor
        content = F.withFocusRing focusRing (E.renderEditor (str . unlines)) contentEditor
     in
        (hLimit 30 (vLimit 5 title) <=> hLimit 30 (vLimit 5 content))

renderLocked :: Note -> Widget Name
renderLocked note =
    hLimit 20 $
    vLimit 5 $
    C.center $
    str (foldr (\note acc -> "* " <> note <> "\n" <> acc) "" (note^.content))

renderMany :: F.FocusRing Name -> E.Editor String Name -> E.Editor String Name -> [Note] -> Widget Name
renderMany focusRing editTitle editContent notes =
    let unlockedWidget = renderUnlocked focusRing editTitle editContent
    in
        B.hBorderWithLabel (str "Existing notes")
        <=> hBox (map (\note -> if _locked note then render note (renderLocked note)  else render note unlockedWidget) notes)
