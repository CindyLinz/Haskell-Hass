{
-- vim: filetype=haskell
{-# LANGUAGE ViewPatterns #-}

module Parser
  ( parseScss
  ) where

import Data.Char

import qualified Data.ByteString as B
import Control.Monad.Trans

import Lexer

parseError :: [Token] -> a
parseError tks = error $ "parseError: " ++ show tks

}

%name parseScss
%tokentype { Token }
%error { parseError }

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
  other_symbol { T_Symbol _ }

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
  other_dir { T_Directive _ }

  w_if { T_Word (map (chr . fromIntegral) . B.unpack -> "if") }
  w_from { T_Word (map (chr . fromIntegral) . B.unpack -> "from") }
  w_to { T_Word (map (chr . fromIntegral) . B.unpack -> "to") }
  w_through { T_Word (map (chr . fromIntegral) . B.unpack -> "through") }
  w_in { T_Word (map (chr . fromIntegral) . B.unpack -> "in") }
  other_word { T_Word _ }

  sp { T_Space _ _ }
  variable { T_Variable _ }

%%

all
  : {}

{


}
