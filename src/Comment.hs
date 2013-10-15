module Comment
  ( Type (..)
  , Important (..)
  ) where

data Type
  = MultiLine
  | SingleLine
  deriving (Show, Eq)

data Important
  = Important
  | NotImportant
  deriving (Show, Eq)
