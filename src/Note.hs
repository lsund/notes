{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Note where

import Prim
import Prelude hiding (unlines, id)
import GHC.Generics
import Field (Field(..))
import qualified Field as F
import Lens.Micro
import Lens.Micro.TH
import Brick.Types (Widget)
import Data.Text (Text, unlines)
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
  , txt
  )

data Note = Note { _id :: Integer, _active :: Bool, _locked :: Bool,  _title :: Field, _content :: Field }
    deriving (Generic, Show)

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

render :: F.FocusRing Id ->  Note -> Widget Id -> Widget Id
render foc note content =
    let active =  (== Just (note ^. id)) (F.focusGetCurrent foc)
     in updateAttrMap (A.applyAttrMappings (borderMappings active)) $
    withBorderStyle BS.ascii $
    B.borderWithLabel (withAttr "title" $ txt (note ^. (title . F.content))) content

renderUnlocked :: F.FocusRing Id -> Note -> Widget Id
renderUnlocked foc note =
    let titleEditor = F.withFocusRing foc (E.renderEditor (txt . unlines)) (note ^. (title . F.editor))  -- TODO
        contentEditor = F.withFocusRing foc (E.renderEditor (txt . unlines)) (note ^. (content . F.editor))
     in
        hLimit 30 (vLimit 5 contentEditor)

renderLocked :: F.FocusRing Id ->Note -> Widget Id
renderLocked foc note =
    hLimit 20 $
    vLimit 5 $
    C.center $
        txt (note^.(content . F.content))
    -- txt (foldr (\note acc -> "* " <> note <> "\n" <> acc) "" (note^.content))

renderMany :: F.FocusRing Id -> [Note] -> Widget Id
renderMany foc notes =
    B.hBorderWithLabel (txt "Existing notes")
    <=> hBox (map
                (\note -> if _locked note then render foc note (renderLocked foc note)  else render foc note (renderUnlocked foc note))
                notes)
