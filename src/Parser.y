{
-- vim: filetype=haskell
{-# LANGUAGE ViewPatterns #-}

module Parser
  ( parseScss
  , runParserT
  ) where

import Data.Char

import qualified Data.ByteString as B
import Control.Monad.Trans

import Lexer

parseError :: Token -> a
parseError tks = error $ "parseError: " ++ show tks

type ParserT = LineNumberKeeperT
runParserT = runLineNumberKeeperT

}

%name parseScss
%tokentype { Token }
%error { parseError }
%monad { ParserT IO }
%lexer { lexer } { T_End }

%token
  ':' { T_Symbol (chr . fromIntegral -> ':') }
  ';' { T_Symbol (chr . fromIntegral -> ';') }
  '!' { T_Symbol (chr . fromIntegral -> '!') }
  '#' { T_Symbol (chr . fromIntegral -> '#') }
  '&' { T_Symbol (chr . fromIntegral -> '&') }

  '{' { T_Symbol (chr . fromIntegral -> '{') }
  '}' { T_Symbol (chr . fromIntegral -> '}') }
  '(' { T_Symbol (chr . fromIntegral -> '(') }
  ')' { T_Symbol (chr . fromIntegral -> ')') }
  '[' { T_Symbol (chr . fromIntegral -> '[') }
  ']' { T_Symbol (chr . fromIntegral -> ']') }

  '"' { T_Symbol (chr . fromIntegral -> '"') }
  '\'' { T_Symbol (chr . fromIntegral -> '\'') }
  '\\' { T_Symbol (chr . fromIntegral -> '\\') }

  '+' { T_Symbol (chr . fromIntegral -> '+') }
  '-' { T_Symbol (chr . fromIntegral -> '-') }
  '*' { T_Symbol (chr . fromIntegral -> '*') }
  '/' { T_Symbol (chr . fromIntegral -> '/') }
  '%' { T_Symbol (chr . fromIntegral -> '%') }

  ',' { T_Symbol (chr . fromIntegral -> ',') }
  '.' { T_Symbol (chr . fromIntegral -> '.') }

  '<' { T_Symbol (chr . fromIntegral -> '<') }
  '>' { T_Symbol (chr . fromIntegral -> '>') }
  '=' { T_Symbol (chr . fromIntegral -> '=') }

  symbol { T_Symbol $$ }

  dir_charset { T_Directive (map (chr . fromIntegral) . B.unpack -> "charset") }

  dir_debug { T_Directive (map (chr . fromIntegral) . B.unpack -> "debug") }
  dir_warn { T_Directive (map (chr . fromIntegral) . B.unpack -> "warn") }

  dir_import { T_Directive (map (chr . fromIntegral) . B.unpack -> "import") }
  dir_extend { T_Directive (map (chr . fromIntegral) . B.unpack -> "extend") }
  dir_include { T_Directive (map (chr . fromIntegral) . B.unpack -> "include") }

  dir_media { T_Directive (map (chr . fromIntegral) . B.unpack -> "media") }
  dir_if { T_Directive (map (chr . fromIntegral) . B.unpack -> "if") }
  dir_else { T_Directive (map (chr . fromIntegral) . B.unpack -> "else") }
  dir_for { T_Directive (map (chr . fromIntegral) . B.unpack -> "for") }
  dir_each { T_Directive (map (chr . fromIntegral) . B.unpack -> "each") }
  dir_while { T_Directive (map (chr . fromIntegral) . B.unpack -> "while") }
  dir_mixin { T_Directive (map (chr . fromIntegral) . B.unpack -> "mixin") }
  dir_function { T_Directive (map (chr . fromIntegral) . B.unpack -> "function") }

  w_if { T_Word (map (chr . fromIntegral) . B.unpack -> "if") }
  w_from { T_Word (map (chr . fromIntegral) . B.unpack -> "from") }
  w_to { T_Word (map (chr . fromIntegral) . B.unpack -> "to") }
  w_through { T_Word (map (chr . fromIntegral) . B.unpack -> "through") }
  w_in { T_Word (map (chr . fromIntegral) . B.unpack -> "in") }

  sp { T_Space $$ }
  directive { T_Directive $$ }
  variable { T_Variable $$ }
  other_word { T_Word $$ }
  comment { T_Comment _ _ _ }

%%

all
  : statements {}

statements
  : sp0 {}
  | sp0 statements_simple {}
  | sp0 statements_complex {}

statement
  : sp {}
  | variable_assign {}

statements_simple
  : statements_simple sp0 ';' sp0 simple_statement {}
  | statements_complex simple_statement {}
  | simple_statement {}

statements_complex
  : statements_simple sp0 ';' sp0 complex_statement {}
  | statements_complex complex_statement {}
  | complex_statement {}

------------------------------

-- statement ended without block { }
simple_statement
  : variable_assign {}
  | property_simple_statement {}

-- statement ended with block { }
complex_statement
  : if_statement {}
  | selector_statement {}

------------------------------
-- simple statements

variable_assign
  : variable sp0 ':' sp0 full_value ';' {}

property_simple_statement
  : selectors full_value ';' {}

----------------------
-- complex statements

block
  : '{' statements '}' {}

selector_statement
  : selectors block sp0 {}

selectors
  : selector selectors {}
  | selector {}
  
selector
  : selector_tag sp0 {}
  | selector_class sp0 {}
  | selector_id sp0 {}
  | selector_pseudo sp0 {}
  | selector_attr sp0 {}

selector_tag
  : word {}

selector_class
  : '.' word {}

selector_id
  : '#' word {}

selector_pseudo
  : ':' word {}

selector_attr
  : '[' word ']' {}

if_statement
  : dir_if sp1 full_value block sp0 {}
  | dir_if sp1 full_value block sp0 dir_else sp0 block {}

----------------------

full_value
  : values sp0 {}

values
  : values sp0 value {}
  | value {}

value
  : interpolation {}
  | variable {}

interpolation
  : '#' '{' sp0 full_value '}' {}

----------------------

sp0
  : {}
  | sp1 {}

sp1
  : sp1_comment {}
  | sp1_sp {}

sp1_comment
  : sp1_sp comment {}
  | comment {}

sp1_sp
  : sp1_comment sp {}
  | sp {}

word
  : other_word {}
  | w_if {}
  | w_from {}
  | w_to {}
  | w_through {}
  | w_in {}

exposeLine :: { Int }
  : {% getLineNumber }

{


}
