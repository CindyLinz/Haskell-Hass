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

  w_if { T_Word (map (chr . fromIntegral) . B.unpack -> "if" }
  w_from { T_Word (map (chr . fromIntegral) . B.unpack -> "from" }
  w_to { T_Word (map (chr . fromIntegral) . B.unpack -> "to" }
  w_through { T_Word (map (chr . fromIntegral) . B.unpack -> "through" }
  w_in { T_Word (map (chr . fromIntegral) . B.unpack -> "in" }

  sp { T_Space $$ }
  directive { T_Directive $$ }
  variable { T_Variable $$ }
  word { T_Word $$ }
  comment { T_Comment _ _ _ }

%%


{


}
