{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module State where

import           Lens.Micro.TH
import           Note

newtype St = St {_notes     :: [Note]}

makeLenses ''St
