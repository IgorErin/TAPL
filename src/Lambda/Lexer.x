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

  "fun"                 { \_ -> TLambda  }

  "->"                  { \_ -> TArrow   }
  "("                   { \_ -> TLParent }
  ")"                   { \_ -> TRParent }
  "true"                { \_ -> TTrue }
  "false"               { \_ -> TFalse }

  "if"                  { \_ -> TIf }
  "then"                { \_ -> TThen }
  "else"                { \_ -> TElse }

  @ident                { TIdent }

{

data Token
  =
  TLParent
  | TRParent

  | TIf
  | TThen
  | TElse

  | TTrue
  | TFalse

  | TLambda
  | TArrow
  | TIdent String
  deriving (Eq, Show)
}
