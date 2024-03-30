{
module Lambda.Parser where

import qualified Lambda.Lexer as L
import qualified Lambda.Expr as LE
import qualified Lambda.Types as TT
import qualified Lambda.Ident as LI
import qualified Lambda.Oper as Op

import Data.List.NonEmpty hiding (reverse)
}

%name run
%tokentype  { L.Token }
%error      { parseError }

%token
   "fun"   { L.TLambda }

   '('      { L.TLParent }
   ')'      { L.TRParent }

   '{'      { L.TLCurlyBrace }
   '}'      { L.TRCurlyBrace }

   '['      { L.TLSquare }
   ']'      { L.TRSquare }

   "->"     { L.TArrow }

   "true"   { L.TTrue }
   "false"  { L.TFalse }
   "()"     { L.TUnit }

   "if"     { L.TIf }
   "then"   { L.TThen }
   "else"   { L.TElse }

   "Bool"   { L.TBoolType }
   "Unit"   { L.TUnitType }
   "Int"    { L.TIntType }

   '_'      { L.TWildCard }

   ':'      { L.TColumn }

   '.'      { L.TDot }
   ','      { L.TComma }

   "as"     { L.TAs }

   "letrec" { L.TLetrec }
   "let"    { L.TLet }
   "in"     { L.TIn }
   '='      { L.TEq }
   int      { L.TInt $$ }

   "+"       { L.TAdd }
   "-"       { L.TSub }
   "*"       { L.TMul }
   ">"       { L.TGt }
   ">="      { L.TGe }
   "<"       { L.TLt }
   "<="      { L.TLe }
   "=="      { L.TEqEq }
   "<>"      { L.TLtGt }

   ident    { L.TIdent $$ }
%%

Program :: { LE.Expr }
Program : Expr                                { $1 }

Expr :: { LE.Expr }
Expr
    : "fun" NEParams "->" Expr                              { LE.lams $2 $4 }
    | "if" Expr "then" Expr "else" Expr                     { LE.if_ $2 $4 $6}
    | "true"                                                { LE.true }
    | "false"                                               { LE.false }
    | "()"                                                  { LE.unit }
    | Expr Expr                                             { LE.app $1 $2 }
    | Var                                                   { $1 }
    | '(' Expr ')'                                          { $2 }
    | Expr "as" TypeExpr                                    { LE.ascription $1 $3 }
    | "let" ident Params '=' Expr "in" Expr                 { LE.let_ $2 $3 $5 $7 }
    | "letrec" ident Params ':' TypeExpr '=' Expr "in" Expr { LE.letrec $2 $3 $5 $7 $9 }
    | RecordExpr                                            { LE.record $1 }
    | VariantExpr                                           { LE.variant $1 }
    | Expr '.' Label                                        { LE.get $1 $3 }
    | int                                                   { LE.int $1 }
    | '(' BinOp Expr Expr ')'                               { LE.binop $3 $2 $4 }

BinOp :: { Op.BinOp }
BinOp
    : "+"                                      { Op.add }
    | "-"                                      { Op.sub }
    | "*"                                      { Op.mul }
    | ">"                                      { Op.gt }
    | ">="                                     { Op.ge }
    | "<"                                      { Op.lt }
    | "<="                                     { Op.le }
    | "=="                                     { Op.eq }
    | "<>"                                     { Op.neq }

Var :: { LE.Expr }
Var : ident                                   { LE.var $1 }

NEParams :: { NonEmpty (LE.Binder, TT.Type) }
    : BinderWithType Params                         { $1 :| $2 }

Params :: { [(LE.Binder, TT.Type)] }
Params : list(BinderWithType)                       { $1 }

Binder :: { LE.Binder }
Binder
    : ident                                         { Just $1 }
    | '_'                                           { Nothing }

BinderWithType :: { (LE.Binder, TT.Type) }
BinderWithType : '(' Binder ':' TypeExpr ')'        { ($2, $4) }

--------------------------- Variant ---------------------------

VariantExpr :: { (LI.Label, LE.Expr) }
VariantExpr : '[' Label '=' Expr ']'                    { ($2, $4) }

VariantType :: { [(LI.Label, TT.Type)] }
VariantType : VariantOf (RecordFiledWithType)        { $1 }

VariantOf (f) : '[' sep (f, ',') ']'                 { $2 }

--------------------------- Record -----------------------------

RecordExpr :: { [(LI.Label, LE.Expr)]}
RecordExpr: RecordOf (RecordInit)                   { $1 }

RecordType :: { [(LI.Label, TT.Type)] }
RecordType : RecordOf (RecordFiledWithType)         { $1 }

RecordOf (f) : '{' sep (f, ',') '}'                 { $2 }

-------------------------- Field -------------------------------

RecordFiledWithType :: { (LI.Label, TT.Type) }
RecordFiledWithType : ident ':' TypeExpr            { ($1, $3) }

RecordInit :: { (LI.Label, LE.Expr) }
RecordInit : ident '=' Expr                         { ($1, $3) }

--------------------------- Types ------------------------------

TypeExpr :: { TT.Type }
TypeExpr
    : SimplType                               { $1 }
    | TypeExpr "->" SimplType                 { TT.arrow $1 $3 }
    | '(' TypeExpr ')'                        { $2 }
    | RecordType                              { TT.record $1 }
    | VariantType                             { TT.variant $1 }

SimplType :: { TT.Type }
SimplType
    : "Bool"                                  { TT.bool }
    | "Unit"                                  { TT.unit }
    | "Int"                                   { TT.int }

----------------------------- specific helpers ----------

Name :: { LI.Name }
Name : ident                                { $1 }

Label :: { LI.Label }
Label : ident                                { $1 }

----------------------------- New helpers -----------------

fst(p, q)        : p q                 { $1 }
snd(p, q)        : p q                 { $2 }
both(p, q)       : p q                 { ($1,$2) }

opt(p)          : p                   { Just $1 }
                |                     { Nothing }

list(p) : rev_list(p)                 { reverse $1 }

rev_list(p)
    : list(p) p                       { $2 : $1 }
    | {- empty -}                     { [] }

non_empty_list(p) : p list(p)         { $1 :| $2 }

sep(p, s)
    : p list(snd(s, p))               { $1 : $2 }
    | {- empty -}                     { [] }

{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"
}
