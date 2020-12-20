{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Field where

import           Brick.Widgets.Edit (Editor)
import           Data.Text          (Text)
import           Lens.Micro.TH

import           Resource

data Field = Field
               { _content :: Text
               , _editor  :: Editor Text Resource
               }
  deriving (Show)

makeLenses ''Field
