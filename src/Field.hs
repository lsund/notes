{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Field where

import           Brick.Widgets.Edit (Editor)
import           Data.Text          (Text)
import           Lens.Micro.TH
import           Prim

data FieldName = Title | Content deriving (Show)

data Field = Field
               { _content :: Text
               , _editor  :: Editor Text Id
               }
  deriving (Show)

makeLenses ''Field
