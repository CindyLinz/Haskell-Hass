{
-- vim: filetype=haskell
{-# LANGUAGE ExistentialQuantification, FlexibleInstances, UndecidableInstances #-}
module EvaluatedSelector where

import qualified Data.ByteString as B
import qualified Data.Word8 as W

newtype Selector = Selector (SpList SimpleChain Comb)

data SimpleChain = SimpleChain (Maybe TypeSel) [SimpleChainElem]

data Comb
  = CombDesc -- space
  | CombChild -- >
  | CombSibAdj -- +
  | CombSibGen -- ~

data TypeSel = TypeSel !B.ByteString
data SimpleChainElem
  = AttrSel !B.ByteString 
  | ClassSel !B.ByteString
  | IDSel !B.ByteString
  | PseudoSel !B.ByteString

data SpList elem sp
  = SpEnd elem
  | SpCons elem sp (SpList elem sp)

instance (Show elem, Show sp) => Show (SpList elem sp) where
  show (SpCons elem sp next) = show elem ++ " " ++ show sp ++ " " ++ show next
  show (SpEnd elem) = show elem ++ " (SpList)"

extend :: Selector -> Selector -> Selector
extend = undefined


data Token
  = T_Word !B.ByteString
  | T_Symbol !W.Word8

lexer :: B.ByteString -> [Token]
lexer bs | B.null bs = []

}

%%
