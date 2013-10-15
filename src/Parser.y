{
  -- vim: filetype=haskell
  module Parser where

  import Lexer
}

%name parseScss
%tokentype { Token }
%error { parseError }

%token
  directive { T_Directive $$ }

%%

code: directive {}

{

  parseError = undefined

}
