module Prim where

data FieldName = Title | Content deriving (Show, Eq, Ord)

formatStr :: String
formatStr = "%d-%m-%Y %l:%M %p"


