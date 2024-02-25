{
module Lambda.Parser where

import qualified Lambda.Lexer as L
import qualified Lambda.Expr as LE
}

%name run
%tokentype  { L.Token }
%error      { parseError }

%token
    "fun"   { L.TLambda }

   '('      { L.TLParent }
   ')'      { L.TRParent }

   '->'     { L.TArrow }

   "true"   { L.TTrue }
   "false"  { L.TFalse }

   "if"     { L.TIf }
   "then"   { L.TThen }
   "else"   { L.TElse }

   ident    { L.TIdent $$ }
%%

Program :: { LE.Expr }
Program : Expr                      { $1 }

Expr :: { LE.Expr }
Expr
    : "fun" ident identList '->' Expr         { LE.lams $2 $3 $5 }
    | "if" Expr "then" Expr "else" Expr       { LE.if_ $2 $4 $6}
    | "true"                                  { LE.true }
    | "false"                                 { LE.false }
    | Expr Expr                               { LE.app $1 $2 }
    | ident                                   { LE.var $1 }
    | '(' Expr ')'                            { $2 }

identList :: { [LE.Symb ]}
identList : identList_                         { reverse $1 }

identList_ :: { [LE.Symb]}
identList_
    : {- empty -}                             { [] }
    | identList ident                         { $2 : $1 }


{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"
}
