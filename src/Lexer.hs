{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Lexer
  ( lexer
  , MonadLineNumberKeeper
  , runLineNumberKeeperT
  , LineNumberKeeperT
  , getLineNumber
  , Token (..)
  ) where

import qualified Data.Word8 as W
import Data.Char
import Control.Monad.State

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
  | T_End
  deriving Eq

instance Show Token where
  show (T_Symbol x) = "T_Symbol " ++ [chr $ fromIntegral x]
  show (T_Directive x) = "T_Directive " ++ show x
  show (T_Variable x) = "T_Variable " ++ show x
  show (T_Placeholder x) = "T_Placeholder " ++ show x
  show (T_Word x) = "T_Word " ++ show x
  show (T_Space x) = "T_Space " ++ show x
  show (T_Comment bs t i) = "T_Comment " ++ show bs ++ " " ++ show t ++ " " ++ show i
  show T_End = "T_End"

class Monad m => MonadLineNumberKeeper m where
  advanceLineNumber :: Int -> m ()
  getStream :: m B.ByteString
  setStream :: B.ByteString -> m ()

type LineNumberKeeperT = StateT (Int, B.ByteString)

runLineNumberKeeperT :: LineNumberKeeperT m a -> Int -> B.ByteString -> m (a, (Int, B.ByteString))
runLineNumberKeeperT m ln bs = runStateT m (ln, bs)

instance Monad m => MonadLineNumberKeeper (LineNumberKeeperT m) where
  advanceLineNumber step = modify $ \(ln, bs) -> (ln+step, bs)
  getStream = get >>= return . snd
  setStream bs = modify $ \(ln, _) -> (ln, bs)

getLineNumber :: Monad m => LineNumberKeeperT m Int
getLineNumber = get >>= return . fst

lexer :: MonadLineNumberKeeper m => (Token -> m a) -> m a
lexer cont = getStream >>= \bs -> case B.uncons bs of
  Nothing -> cont T_End
  Just (first, bs') ->
    if isSpace first then
      let (spaces, bs'') = B.span isSpace bs
      in setStream bs'' >> advanceLineNumber (B.count W._lf spaces) >> cont (T_Space spaces)
    else if W._hyphen == first && B.null bs' then
      setStream bs' >> cont (T_Symbol W._hyphen)
    else if isWord first then
      takeWord T_Word bs
    else case B.uncons bs' of
      Nothing -> setStream bs' >> cont (T_Symbol first)
      Just (second, bs'') -> case isWord second of
        True
          | first == W._at -> takeIDWord T_Directive bs'
          | first == W._percent -> takeIDWord T_Placeholder bs'
          | first == W._dollar -> takeIDWord T_Variable bs'
          | otherwise -> fallback
        False
          | first == W._slash ->
            let
              T_Comment comment _ _ = result
              importance = if maybe False ((== W._exclam) . fst) (B.uncons comment)
                then Comment.Important
                else Comment.NotImportant
              (result, remain, lnStep)
                | second == W._slash =
                  let
                    (c, later) = B.break (== W._lf) bs''
                    bs''' = B.tail later
                  in (T_Comment c Comment.SingleLine importance, bs''', 1)
                | second == W._asterisk =
                  let
                    (c, later) = B.breakSubstring (B.pack [W._asterisk, W._slash]) bs''
                    bs''' = B.drop 2 later
                  in (T_Comment c Comment.MultiLine importance, bs''', B.count W._lf c)
                | otherwise = (T_Symbol first, bs', 0)
            in setStream remain >> advanceLineNumber lnStep >> cont result
          | otherwise -> fallback
      where
        fallback = setStream bs' >> cont (T_Symbol first)
    where 
      isIDWord x = isWord x || x == W._hyphen
      isWord x = W.isAlpha x || W.isDigit x || x == W._underscore || x > 127
      isSpace x = x <= 127 && W.isSpace x
      takeWord = takeWordWith isWord
      takeIDWord = takeWordWith isIDWord
      takeWordWith pred con bs = setStream bs' >> cont (con word)
        where (word, bs') = B.span pred bs
