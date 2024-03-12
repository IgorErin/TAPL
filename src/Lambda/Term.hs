module Lambda.Term (Term(..), Field, Record) where

import Lambda.Types(Type)
import Lambda.Ident (Index, Label)

type Info = Maybe String

infixl 4 :@:

type Field = (Label, Term)
type Record = [Field]

data Term =
    Idx Index
    | Tru
    | Fls
    | Unit
    | Int Int
    | If Term Term Term
    | Term :@: Term
    | Lmb Info Type Term
    | Record Record
    | Get Term Label
    deriving (Eq, Show)