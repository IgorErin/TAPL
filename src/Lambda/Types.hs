module Lambda.Types (
    Type(..),
    Field,
    Record,
    bool, unit,
    arrow,
    record
) where

import Lambda.Ident (Label)

infixr 3 :->

type Field = (Label, Type)
type Record = [Field]

data Type =
    Bool
    | Unit
    | Type :-> Type
    | Record Record
    deriving (Show, Eq)

bool :: Type
bool = Bool

unit :: Type
unit = Unit

arrow :: Type -> Type -> Type
arrow f s = f :-> s

record :: [(Label, Type)] -> Type
record = Record