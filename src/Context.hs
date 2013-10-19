module Context where

import qualified Data.Map as M
import qualified Data.ByteString as B

data Context
  { ctxVariable :: M.Map B.ByteString Value
  , ctxFunction :: M.Map B.ByteString Function
  , ctxMixin :: M.Map B.ByteString Mixin
  }

type Value = B.ByteString

data Mixin = Mixin
  { mixinParam :: M.Map B.ByteString
