module Arith.Ast (
    Term(..),
    true, false,
    zero, succ_, pred_,
    if_, isZero
    ) where

data Term =
    TTrue
    | TFalse
    | TIf Term Term Term
    | TZero
    | TSucc Term
    | TPred Term
    | TIsZero Term
    deriving (Show, Eq)

true :: Term
true = TTrue

false :: Term
false = TFalse

if_ :: Term -> Term -> Term -> Term
if_ = TIf

zero :: Term
zero = TZero

succ_ :: Term -> Term
succ_ = TSucc

pred_ :: Term -> Term
pred_ = TPred

isZero :: Term -> Term
isZero = TIsZero
