{
module Lambda.Lexer(alexScanTokens, Token (..)) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$backSlash = \

@ident = $alpha [$alpha $digit]*
@number = digit+

tokens :-
  $white+               ;
  $digit+              { toInt }

  "fun"                 { \_ -> TLambda  }

  "isZero"              { \_ -> TIsZero }
  "succ"                { \_ -> TSucc }
  "pred"                { \_ -> TPred}

  "->"                  { \_ -> TArrow   }

  "("                   { \_ -> TLParent }
  ")"                   { \_ -> TRParent }

  "{"                   { \_ -> TLCurlyBrace }
  "}"                   { \_ -> TRCurlyBrace }

  "["                   { \_ -> TLSquare }
  "]"                   { \_ -> TRSquare }

  "true"                { \_ -> TTrue }
  "false"               { \_ -> TFalse }
  "()"                  { \_ -> TUnit}

  "if"                  { \_ -> TIf }
  "then"                { \_ -> TThen }
  "else"                { \_ -> TElse }

  "Bool"                { \_ -> TBoolType }
  "Unit"                { \_ -> TUnitType }
  "Int"                 { \_ -> TIntType }

  "_"                   { \_ -> TWildCard }

  ":"                   { \_ -> TColumn }

  "as"                  { \_ -> TAs }

  "letrec"              { \_ -> TLetrec }
  "let"                 { \_ -> TLet}
  "in"                  { \_ -> TIn }
  "="                   { \_ -> TEq }

  "match"               { \_ -> TMatch }
  "with"                { \_ -> TWith }
  "|"                   { \_ -> TVerBar }
  "end"                 { \_ -> TEnd }

  ","                   { \_ -> TComma }
  "."                   { \_ -> TDot }

  @ident                { TIdent }

{
toInt = TInt . read

data Token
  =
  -- ( )
  TLParent
  | TRParent

  -- { }
  | TRCurlyBrace
  | TLCurlyBrace

  -- [ ]
  | TRSquare
  | TLSquare

  | TColumn
  | TDot
  | TComma

  | TMatch
  | TWith
  | TVerBar
  | TEnd

  -- if then else
  | TIf
  | TThen
  | TElse
  -- ascription
  | TAs
  -- let in
  | TLetrec
  | TLet
  | TIn
  | TEq
  -- values
  | TTrue
  | TFalse
  | TInt Int
  | TUnit
  -- builtin types
  | TBoolType
  | TUnitType
  | TIntType

  | TIsZero
  | TSucc
  | TPred

  | TWildCard

  | TLambda
  | TArrow
  | TIdent String
  deriving (Eq, Show)
}
