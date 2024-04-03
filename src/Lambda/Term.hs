module Lambda.Term (Term(..), Field, Record) where

import Lambda.Ident (Label)
import Lambda.Index (Index)
import Lambda.Oper (UnOp)
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
    | UnOp UnOp Term
    | If Term Term Term
    | Term :@: Term
    | Lmb Info Term
    | Record Record
    | Variant Field
    | Get Term Label
    | CaseOf Term [(Pattern, Term)]
    | Fix Term
    deriving (Eq, Show)