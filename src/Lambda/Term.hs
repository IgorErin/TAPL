module Lambda.Term (Term(..), Field, Record) where

import Lambda.Types(Type)
import Lambda.Ident (Label)
import Lambda.Index (Index)
import Lambda.Oper (BinOp)
import Lambda.Pattern (Pattern)

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
    | BinOp Term BinOp Term
    | If Term Term Term
    | Term :@: Term
    | Lmb Info Type Term
    | Record Record
    | Variant Field
    | Get Term Label
    | CaseOf Term [(Pattern, Term)]
    | Fix Term
    deriving (Eq, Show)