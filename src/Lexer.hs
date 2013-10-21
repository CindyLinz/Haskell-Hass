{-# LANGUAGE MultiWayIf, OverloadedStrings #-}
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
  = T_Symbol W.Word8
  | T_Directive B.ByteString
  | T_Variable B.ByteString
  | T_Placeholder B.ByteString
  | T_Word B.ByteString
  | T_Space B.ByteString Int
  | T_Comment B.ByteString Comment.Type Comment.Important Int

  | T_StringBegin !W.Word8
  | T_StringEnd
  | T_StringBody !B.ByteString

  | T_InterpBegin
  | T_InterpEnd

  | T_Error B.ByteString
  deriving Eq

data Mode
  = CodeMode !Bool -- True for interp, False otherwise
  | StringMode !W.Word8
  deriving (Show, Eq)

instance Show Token where
  show (T_Symbol x) = "T_Symbol " ++ [chr $ fromIntegral x]
  show (T_Directive x) = "T_Directive " ++ show x
  show (T_Variable x) = "T_Variable " ++ show x
  show (T_Placeholder x) = "T_Placeholder " ++ show x
  show (T_Word x) = "T_Word " ++ show x
  show (T_Space x n) = "T_Space " ++ show x ++ " (" ++ show n ++ ")"
  show (T_Comment bs t i n) = "T_Comment " ++ show bs ++ " " ++ show t ++ " " ++ show i ++ " (" ++ show n ++ ")"

lexer :: B.ByteString -> [Token]
lexer = go [CodeMode False] where
  go :: [Mode] -> B.ByteString -> [Token]
  go allModes@(mode:modes) bs = case B.uncons bs of
    Nothing -> if
      | allModes == [CodeMode False] -> []
      | otherwise -> T_Error (fromString ("lexer mode stack not matched: " ++ show allModes)) : []
    Just (first, bs') -> case mode of
      CodeMode interpolating -> if
        | isSpace first ->
          let
            (spaces, bs'') = B.span isSpace bs
            lineCount = B.count (c '\n') spaces
          in T_Space spaces lineCount : lexer bs''
        | c '-' == first && B.null bs' ->
          T_Symbol (c '-') : []
        | c '"' == first || c '\'' == first ->
          T_StringBegin first : go (StringMode first : allModes) bs'
        | c '}' == first ->
          ( if interpolating then T_InterpEnd else T_Symbol (c '}') ) : go modes bs'
        | c '{' == first ->
          T_Symbol (c '{') : go (CodeMode False : allModes) bs'
        | isWord first ->
          takeWord T_Word bs
        | otherwise -> case B.uncons bs' of
          Nothing -> T_Symbol first : []
          Just (second, bs'') -> if
            | isWord second -> if
              | c '@' == first -> takeIDWord T_Directive bs'
              | c '%' == first -> takeIDWord T_Placeholder bs'
              | c '$' == first -> takeIDWord T_Variable bs'
              | otherwise -> fallback
            | otherwise -> if
              | c '#' == first && c '{' == second ->
                T_InterpBegin : go (CodeMode True : allModes) bs''
              | c '/' == first ->
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
                      in T_Comment content Comment.SingleLine importance 1 : lexer bs'''
                    | second == c '*' =
                      let
                        (content, later) = B.breakSubstring (B.pack (map c "*/")) bs''
                        bs''' = B.drop 2 later
                        lineCount = B.count (c '\n') content
                      in T_Comment content Comment.MultiLine importance lineCount : lexer bs'''
                    | otherwise = fallback
                in result
              | otherwise -> fallback
          where
            fallback = T_Symbol first : lexer bs'
      StringMode delimiter ->
        let
          (former, later) = B.span (\ch -> delimiter == ch || c '#' == ch || c '\\' == ch) bs
          res = if B.null former then resLater else T_StringBody former : resLater
          resLater = case B.uncons later of
            Nothing -> T_Error "un-closed string" : []
            Just (later1, later') -> if
              | later1 == delimiter -> T_StringEnd : go modes later'
              | otherwise -> case B.uncons later' of
                Nothing -> T_StringBody later : go allModes B.empty
                Just (later2, later'') -> if
                  | c '#' == later1 && c '{' == later2 -> T_InterpBegin : go (CodeMode True : allModes) later''
                  | c '\\' == later2 || c '"' == later2 || c '\'' == later2 -> T_StringBody (B.singleton later2) : go allModes later''
                  | otherwise -> T_StringBody (B.pack [later1, later2]) : go allModes later''
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
