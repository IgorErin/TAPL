{
module Lambda.Parser where

import qualified Lambda.Lexer as L
import qualified Lambda.Expr as LE
import qualified Lambda.Types as TT

import Data.List.NonEmpty hiding (reverse)
}

%name run
%tokentype  { L.Token }
%error      { parseError }

%token
    "fun"   { L.TLambda }

   '('      { L.TLParent }
   ')'      { L.TRParent }

   "->"     { L.TArrow }

   "true"   { L.TTrue }
   "false"  { L.TFalse }
   "()"     { L.TUnit }

   "if"     { L.TIf }
   "then"   { L.TThen }
   "else"   { L.TElse }

   "Bool"   { L.TBoolType }
   "Unit"   { L.TUnitType }

   ':'      { L.TColumn }

   ident    { L.TIdent $$ }
%%

Program :: { LE.Expr }
Program : Expr                      { $1 }

Expr :: { LE.Expr }
Expr
    : "fun" Params "->" Expr                  { LE.lams $2 $4 }
    | "if" Expr "then" Expr "else" Expr       { LE.if_ $2 $4 $6}
    | "true"                                  { LE.true }
    | "false"                                 { LE.false }
    | "()"                                    { LE.unit }
    | Expr Expr                               { LE.app $1 $2 }
    | ident                                   { LE.var $1 }
    | '(' Expr ')'                            { $2 }

Params : IdentWithType List(IdentWithType)    { $1 :| $2 }

IdentWithType :: { (LE.Ident, TT.Type) }
IdentWithType : '(' ident ':' TypeExpr ')'    { ($2, $4) }

--------------------------- Types ------------------------------

TypeExpr :: { TT.Type }
TypeExpr
    : SimplType                               { $1 }
    | TypeExpr "->" SimplType                 { TT.arrow $1 $3 }
    | '(' TypeExpr ')'                        { $2 }

SimplType :: { TT.Type }
SimplType
    : "Bool"                                  { TT.bool }
    | "Unit"                                  { TT.unit }

---------------------------- Helpers -------------------------

List(p) : RevList(p)                          { reverse $1 }

RevList(p)
    : RevList(p) p                            { $2 : $1 }
    | {- empty -}                             { [] }

{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"
}
