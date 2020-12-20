{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module State where

import qualified Brick.Focus   as F
import           Lens.Micro.TH
import           Note
import           Prim

data St = St
            { _focusRing :: F.FocusRing Id
            , _notes     :: [Note]
            }

makeLenses ''St
