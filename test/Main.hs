module Main
  ( main
  ) where

import qualified Data.ByteString as B
import Control.Monad.Trans.Class

import Lexer

main = do
  src <- B.getContents
  let
    go = lexer $ \token -> do
      ln <- getLineNumber
      lift $ putStrLn $ show token ++ " ..line " ++ show ln
      if token == T_End
        then return ()
        else go
  runLineNumberKeeperT go 1 src
