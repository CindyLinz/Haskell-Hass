module Lexer
  ( lexer
  , Token (..)
  ) where

import qualified Data.Word8 as W
import Data.Char

import qualified Data.ByteString as B

data Token
  = T_Symbol W.Word8
  | T_Directive B.ByteString
  | T_Variable B.ByteString
  | T_Placeholder B.ByteString
  | T_Word B.ByteString
  | T_Space B.ByteString Int

  | T_FileBegin
  | T_FileEnd
  | T_InterpolateBegin
  | T_InterpolateEnd
  | T_StringBegin W.Word8
  | T_StringEnd

instance Show Token where
  show (T_Symbol x) = "T_Symbol " ++ [chr $ fromIntegral x]
  show (T_Directive x) = "T_Directive " ++ show x
  show (T_Variable x) = "T_Variable " ++ show x
  show (T_Placeholder x) = "T_Placeholder " ++ show x
  show (T_Word x) = "T_Word " ++ show x
  show (T_Space x n) = "T_Space " ++ show x ++ " (" ++ show n ++ ")"

lexer :: B.ByteString -> [Token]
lexer bs = case B.uncons bs of
  Nothing -> []
  Just (first, bs') ->
    if isSpace first then
      let
        (spaces, bs'') = B.span isSpace bs
        lineCount = B.count W._lf spaces
      in T_Space spaces lineCount : lexer bs''
    else if W._hyphen == first && B.null bs' then
      T_Symbol W._hyphen : []
    else if isWord first then
      takeWord T_Word bs
    else case B.uncons bs' of
      Nothing -> T_Symbol first : []
      Just (second, bs'') -> case isWord second of
        True
          | first == W._at -> takeIDWord T_Directive bs'
          | first == W._percent -> takeIDWord T_Placeholder bs'
          | first == W._dollar -> takeIDWord T_Variable bs'
          | otherwise -> fallback
        False
          | otherwise -> fallback
      where
        fallback = T_Symbol first : lexer bs'
    where 
      isIDWord x = isWord x || x == W._hyphen
      isWord x = W.isAlpha x || W.isDigit x || x == W._underscore || x > 127
      isSpace x = x <= 127 && W.isSpace x
      takeWord = takeWordWith isWord
      takeIDWord = takeWordWith isIDWord
      takeWordWith pred con bs = con word : lexer bs'
        where (word, bs') = B.span pred bs
