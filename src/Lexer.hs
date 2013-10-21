{-# LANGUAGE OverloadedStrings #-}
module Lexer
  ( lexer
  , Token (..)
  ) where

import qualified Data.Word8 as W
import Data.Char
import Data.String

import qualified Data.ByteString as B

import qualified Comment

data Token
  = T_Symbol !W.Word8
  | T_Directive !B.ByteString
  | T_Variable !B.ByteString
  | T_Placeholder !B.ByteString
  | T_Word !B.ByteString
  | T_Space !B.ByteString !Int
  | T_Comment !B.ByteString !Comment.Type Comment.Important !Int

  | T_StringBegin !W.Word8
  | T_StringEnd
  | T_StringBody !B.ByteString !Int

  | T_InterpBegin
  | T_InterpEnd

  | T_Error !B.ByteString
  | T_Debug !B.ByteString
  deriving Eq

instance Show Token where
  show (T_Symbol x) = "T_Symbol " ++ [chr $ fromIntegral x]
  show (T_Directive x) = "T_Directive " ++ show x
  show (T_Variable x) = "T_Variable " ++ show x
  show (T_Placeholder x) = "T_Placeholder " ++ show x
  show (T_Word x) = "T_Word " ++ show x
  show (T_Space x n) = "T_Space " ++ show x ++ " (" ++ show n ++ ")"
  show (T_Comment bs t i n) = "T_Comment " ++ show bs ++ " " ++ show t ++ " " ++ show i ++ " (" ++ show n ++ ")"
  show (T_StringBegin del) = "T_StringBegin(" ++ [chr $ fromIntegral del] ++ ")"
  show T_StringEnd = "T_StringEnd"
  show (T_StringBody bs n) = "T_StringBody " ++ show bs ++ " (" ++ show n ++ ")"
  show T_InterpBegin = "T_InterpBegin"
  show T_InterpEnd = "T_InterpEnd"
  show (T_Error bs) = "T_Error " ++ show bs
  show (T_Debug bs) = "T_Debug " ++ show bs

data Mode
  = CodeMode !Bool -- True for interp, False otherwise
  | StringMode !W.Word8
  deriving Eq

instance Show Mode where
  show (CodeMode interp) = "CodeMode(" ++ show interp ++ ")"
  show (StringMode del) = "StringMode(" ++ [chr $ fromIntegral del] ++ ")"

lexer :: B.ByteString -> [Token]
lexer = go [CodeMode False] where
  go :: [Mode] -> B.ByteString -> [Token]
  go [] _ = T_Error "lexer mod stack is emptied" : []
  go allModes@(mode:modes) bs = case B.uncons bs of
    Nothing -> case () of
      _| allModes == [CodeMode False] -> []
      _| otherwise -> T_Error (fromString ("lexer mode stack not matched: " ++ show allModes)) : []
    Just (first, bs') -> case mode of
      CodeMode interpolating -> case () of
        _| isSpace first ->
          let
            (spaces, bs'') = B.span isSpace bs
            lineCount = B.count (c '\n') spaces
          in T_Space spaces lineCount : go allModes bs''
        _| c '-' == first && B.null bs' ->
          T_Symbol (c '-') : []
        _| c '"' == first || c '\'' == first ->
          T_StringBegin first : go (StringMode first : allModes) bs'
        _| c '}' == first ->
          ( if interpolating then T_InterpEnd else T_Symbol (c '}') ) : go modes bs'
        _| c '{' == first ->
          T_Symbol (c '{') : go (CodeMode False : allModes) bs'
        _| isWord first ->
          takeWord T_Word bs
        _| True -> let fallback = T_Symbol first : go allModes bs'
          in case B.uncons bs' of
            Nothing -> T_Symbol first : []
            Just (second, bs'') -> case () of
              _| isWord second -> case () of
                _| c '@' == first -> takeIDWord T_Directive bs'
                _| c '%' == first -> takeIDWord T_Placeholder bs'
                _| c '$' == first -> takeIDWord T_Variable bs'
                _| True -> fallback
              _| True -> case () of
                _| c '#' == first && c '{' == second ->
                  T_InterpBegin : go (CodeMode True : allModes) bs''
                _| c '/' == first ->
                  let
                    T_Comment comment _ _ _ = head result
                    importance = if maybe False ((== c '!') . fst) (B.uncons comment)
                      then Comment.Important
                      else Comment.NotImportant
                    result
                      | second == c '/' =
                        let
                          (content, later) = B.break (== c '\n') bs''
                          bs''' = B.tail later
                        in T_Comment content Comment.SingleLine importance 1 : go allModes bs'''
                      | second == c '*' =
                        let
                          (content, later) = B.breakSubstring (B.pack (map c "*/")) bs''
                          bs''' = B.drop 2 later
                          lineCount = B.count (c '\n') content
                        in T_Comment content Comment.MultiLine importance lineCount : go allModes bs'''
                      | True = fallback
                  in result
                _| True -> fallback
      StringMode delimiter ->
        let
          makeStringBody bs = T_StringBody bs (B.count (c '\n') bs)
          (former, later) = B.break (\ch -> delimiter == ch || c '#' == ch || c '\\' == ch) bs
          res = if B.null former then resLater else makeStringBody former : resLater
          resLater = case B.uncons later of
            Nothing -> T_Error "un-closed string" : []
            Just (later1, later') -> case () of
              _| later1 == delimiter -> T_StringEnd : go modes later'
              _| otherwise -> case B.uncons later' of
                Nothing -> makeStringBody later : go allModes B.empty
                Just (later2, later'') -> case () of
                  _| c '#' == later1 && c '{' == later2 -> T_InterpBegin : go (CodeMode True : allModes) later''
                  _| c '\\' == later2 || c '"' == later2 || c '\'' == later2 -> T_StringBody (B.singleton later2) 0 : go allModes later''
                  _| otherwise -> makeStringBody (B.pack [later1, later2]) : go allModes later''
        in res
      where 
        isIDWord x = isWord x || x == c '-'
        isWord x = W.isAlpha x || W.isDigit x || x == c '_' || x > 127
        isSpace x = x <= 127 && W.isSpace x
        takeWord = takeWordWith isWord
        takeIDWord = takeWordWith isIDWord
        takeWordWith pred con bs = con word : go allModes bs'
          where (word, bs') = B.span pred bs

c :: Char -> W.Word8
c = fromIntegral . ord
