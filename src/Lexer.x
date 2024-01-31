{
-- At the top of the file, we define the module and its imports, similarly to Haskell.
module Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}
-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

<0> $white+ ;

-- Keywords
<0> let     { tok Let }
<0> in      { tok In }
<0> if      { tok If }
<0> then    { tok Then }
<0> else    { tok Else }

-- Arithmetic operators
<0> "+"     { tok Plus }
<0> "-"     { tok Minus }
<0> "*"     { tok Times }
<0> "/"     { tok Divide }

-- Comparison operators
<0> "="     { tok Eq }
<0> "<>"    { tok Neq }
<0> "<"     { tok Lt }
<0> "<="    { tok Le }
<0> ">"     { tok Gt }
<0> ">="    { tok Ge }

-- Logical operators
<0> "&"     { tok And }
<0> "|"     { tok Or }

-- Parenthesis
<0> "("     { tok LPar }
<0> ")"     { tok RPar }

-- Lists
<0> "["     { tok LBrack }
<0> "]"     { tok RBrack }
<0> ","     { tok Comma }

-- Types
<0> ":"     { tok Colon }
<0> "->"    { tok Arrow }

-- Identifiers
<0> @id     { tokId }
-- Constants
<0> $digit+ { tokInteger }

{
-- At the bottom, we may insert more Haskell definitions, such as data structures, auxiliary functions, etc.
data AlexUserState = AlexUserState
  {
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState

alexEOF :: Alex RangedToken
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show)

data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

data Token
  -- Identifiers
  = Identifier ByteString
  -- Constants
  | String ByteString
  | Integer Integer
  -- Keywords
  | Let
  | In
  | If
  | Then
  | Else
  -- Arithmetic operators
  | Plus
  | Minus
  | Times
  | Divide
  -- Comparison operators
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  -- Logical operators
  | And
  | Or
  -- Parenthesis
  | LPar
  | RPar
  -- Lists
  | Comma
  | LBrack
  | RBrack
  -- Types
  | Colon
  | Arrow
  -- EOF
  | EOF
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Identifier $ BS.take len str
    , rtRange = mkRange inp len
    }

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Integer $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }
}