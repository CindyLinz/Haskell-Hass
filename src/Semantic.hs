module Semantic where

data Value
  = AnyValue !B.ByteString
  | StringValue !B.ByteString -- value that produced by interpolation #{...}

asString :: Value -> B.ByteString
asString (AnyValue bs) = bs
asString (StringValue bs) = bs

as
