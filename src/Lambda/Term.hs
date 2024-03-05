module Lambda.Term (Term(..), Ident) where

import Lambda.Types(Type)

type Ident = Int
type Info = String

infixl 4 :@:

data Term =
    Idx Ident
    | Tru
    | Fls
    | Unit
    | If Term Term Term
    | Term :@: Term
    | Lmb Info Type Term
    deriving (Eq, Read, Show)