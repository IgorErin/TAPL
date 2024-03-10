module Lambda.Types (
    Type(..),
    bool, unit,
    arrow
) where

import Lambda.Ident (Label)

infixr 3 :->

data Type =
    Bool
    | Unit
    | Type :-> Type
    | Record (Label, Type)
    deriving (Show, Eq)

bool :: Type
bool = Bool

unit :: Type
unit = Unit

arrow :: Type -> Type -> Type
arrow f s = f :-> s