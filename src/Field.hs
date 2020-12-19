{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric     #-}
module Field where

import Data.Text (Text)
import qualified Brick.Widgets.Edit as E
import Lens.Micro.TH
import Prim

data Field = Field { _content :: Text, _editor :: E.Editor Text Id }
    deriving (Show)

makeLenses ''Field
