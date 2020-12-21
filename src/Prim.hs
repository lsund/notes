module Prim where

data FieldName = Title | Content deriving (Show, Eq, Ord)
type Id = Integer

formatStr :: String
formatStr = "%d-%m-%Y %l:%M %p"


