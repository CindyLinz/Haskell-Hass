module Main where

import qualified Data.ByteString as B
import Control.Monad.Trans

import Lexer
import Parser

testLexer = do
  src <- B.getContents
  let
    go = lexer $ \token -> do
      ln <- getLineNumber
      lift $ putStrLn $ show token ++ " ..line " ++ show ln
      if token == T_End
        then return ()
        else go
  runLineNumberKeeperT go 1 src

testParser = do
  src <- B.getContents
  runParserT parseScss 1 src

main = testParser
