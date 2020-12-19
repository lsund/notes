{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric     #-}

module State where

import Lens.Micro.TH
import qualified Brick.Focus as F
import Note
import Prim

data St =
    St { _focusRing :: F.FocusRing Id
       , _notes :: [Note]
       }

makeLenses ''St
