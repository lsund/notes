{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Resource where

import           Lens.Micro.TH

import           Prim

data Resource = Resource
                  { _noteid    :: Int
                  , _fieldName :: FieldName
                  }
  deriving (Show, Eq, Ord)

allResources :: [Int] -> [Resource]
allResources xs = map (`Resource` Title) xs ++ map (`Resource` Content) xs

makeLenses ''Resource

