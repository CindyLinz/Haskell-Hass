module Main
  ( main
  ) where

import qualified Data.ByteString as B

import Lexer

main = do
  src <- B.getContents
  putStrLn $ show $ lexer src
