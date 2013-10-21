module Context where

import qualified Data.Map as M
import qualified Data.ByteString as B

--data Context
--  { ctxVariable :: M.Map B.ByteString Value
--  , ctxFunction :: M.Map B.ByteString Function
--  , ctxMixin :: M.Map B.ByteString Mixin
--  }

data Context a
