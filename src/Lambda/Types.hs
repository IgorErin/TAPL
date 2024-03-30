module Lambda.Types (
    Type(..),
    Field,
    Record,
    bool, unit, int,
    arrow,
    record,
    variant
) where

import Lambda.Ident (Label)

infixr 3 :->

type Field = (Label, Type)

type Record = [Field]
type Variant = [Field]

data Type =
    Bool
    | Unit
    | Int
    | Type :-> Type
    | Record Record
    | Variant Variant
    deriving (Show, Eq)

bool :: Type
bool = Bool

unit :: Type
unit = Unit

int :: Type
int = Int

arrow :: Type -> Type -> Type
arrow f s = f :-> s

record :: Record -> Type
record = Record

variant :: Variant -> Type
variant = Variant