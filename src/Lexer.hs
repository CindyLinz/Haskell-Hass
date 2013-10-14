module Lexer
  ( lexer
  ) where

import qualified Data.Word8 as W

import qualified Data.ByteString as B

import qualified Comment

data Token
  = T_Symbol W.Word8
  | T_Directive B.ByteString
  | T_Variable B.ByteString
  | T_Placeholder B.ByteString
  | T_Word B.ByteString
  | T_Space B.ByteString
  | T_Comment B.ByteString Comment.Type Comment.Important
  deriving Show

lexer :: B.ByteString -> [Token]
lexer bs = case B.uncons bs of
  Nothing -> []
  Just (first, bs') ->
    if isSpace first then
      let (spaces, bs'') = B.span isSpace bs
      in T_Space spaces : lexer bs''
    else if W._hyphen == first && B.null bs' then
      T_Symbol W._hyphen : []
    else if isWord first then
      takeWord T_Word bs
    else case B.uncons bs' of
      Nothing -> T_Symbol first : []
      Just (second, bs'') -> case isWord second of
        True
          | first == W._at -> takeWord T_Directive bs'
          | first == W._percent -> takeWord T_Placeholder bs'
          | first == W._dollar -> takeWord T_Variable bs'
          | first == W._slash ->
            let
              T_Comment comment _ _ = head result
              importance = if maybe False ((== W._exclam) . fst) (B.uncons comment)
                then Comment.Important
                else Comment.NotImportant
              result
                | second == W._slash =
                  let
                    (c, later) = B.break (== W._lf) bs''
                    bs''' = B.tail later
                  in T_Comment c Comment.SingleLine importance : lexer bs'''
                | second == W._asterisk =
                  let
                    (c, later) = B.breakSubstring bs'' (B.pack [W._asterisk, W._slash])
                    bs''' = B.drop 2 later
                  in T_Comment c Comment.MultiLine importance : lexer bs'''
                | otherwise = fallback
            in result
          | otherwise -> fallback
        _ -> fallback
      where
        fallback = T_Symbol first : lexer bs'
    where 
      isWord x = W.isAlpha x || W.isDigit x || x == W._underscore || x == W._hyphen || x > 127
      isSpace x = x <= 127 && W.isSpace x
      takeWord con bs = con word : lexer bs'
        where (word, bs') = B.span isWord bs
