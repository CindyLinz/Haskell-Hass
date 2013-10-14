module Comment
  ( Type (..)
  , Important (..)
  ) where

data Type
  = MultiLine
  | SingleLine
  deriving Show

data Important
  = Important
  | NotImportant
  deriving Show
