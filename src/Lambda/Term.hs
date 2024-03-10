module Lambda.Term (Term(..)) where

import Lambda.Types(Type)
import Lambda.Ident (Index, Label)

type Info = Maybe String

infixl 4 :@:

data Term =
    Idx Index
    | Tru
    | Fls
    | Unit
    | If Term Term Term
    | Term :@: Term
    | Lmb Info Type Term
    | Record [(Label, Term)] Type
    deriving (Eq, Show)