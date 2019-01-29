{
module Parser where

import Lyambda
import Lexer
}

%name      parseExpr
%tokentype { Token }
%error     { parseError }
%monad     { Either String }{ >>= }{ return }

%token IDENT   { Ident $$ }
%token DOT     { DotT }
%token LYAMBDA { LyambdaT }
%token LEFTP   { LeftP }
%token RIGHTP  { RightP }

%%

Expression
  : App                      			{ $1 }
  | App LYAMBDA Var DOT Expression 		{ Ap $1 (Ab $3 $5) }
  | LYAMBDA Var DOT Expression 		{ Ab $2 $4 }

App
  : Atom               					{ $1 }
  | App Atom       	   					{ Ap $1 $2 }

Atom
  : Var               	    			{ $1 }
  | LEFTP Expression RIGHTP 			{ $2 }

Var
  : IDENT              					{ Var $1 }

{
parseError = fail "Parse error"
}
