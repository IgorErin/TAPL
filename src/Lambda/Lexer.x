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

  @ident                { TIdent }

{

data Token
  =
  TLParent
  | TRParent

  | TLambda
  | TArrow
  | TIdent String
  deriving (Eq, Show)
}
