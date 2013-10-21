{-# LANGUAGE ExistentialQuantification, FlexibleInstances, UndecidableInstances #-}
module SyntaxTree where

import qualified Data.ByteString as B

import qualified Context as C

data SpList elem sp
  = SpEnd elem
  | SpCons elem sp (SpList elem sp)

data FunctionDecl

data Mixin

data Include

data Import

data VarAssign = VarAssign !B.ByteString !Value

data PropertyAssign

data SelectorAssign = SelectorAssign !Selector !Block

newtype Selector = Selector (SpList SelSimpleChain SelComb)

data SelComb
  = SelCombDesc -- space
  | SelCombChild -- >
  | SelCombSibAdj -- +
  | SelCombSibGen -- ~

data SelSimpleChain
  = SelSimpleChain (Maybe TypeSel) [SelSimpleChainElem]

data TypeSel = TypeSel !B.ByteString
data SelSimpleChainElem
  = AttrSel !B.ByteString 

data Block = forall s. IsStatement s => Block [s]

data Value = forall v. IsValue v => Value [v]
newtype WordValue = WordValue B.ByteString
data StringValue = forall v. IsInStringValue v => StringValue [v]
newtype InterpValue = InterpValue Value

class IsStatement s where
instance IsStatement FunctionDecl where
instance IsStatement Mixin where
instance IsStatement Include where
instance IsStatement Import where
instance IsStatement VarAssign where
instance IsStatement PropertyAssign where
instance IsStatement SelectorAssign where

class IsValue v where
instance IsInStringValue v => IsValue v where
instance IsValue StringValue where

class IsInStringValue v where
instance IsInStringValue WordValue where
instance IsInStringValue InterpValue where
