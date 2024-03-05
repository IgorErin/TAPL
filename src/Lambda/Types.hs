module Lambda.Types (
    Type(..),
    bool, unit,
    arrow
) where

infixr 3 :->

data Type =
    Bool
    | Unit
    | Type :-> Type
    deriving (Read, Show, Eq)

bool :: Type
bool = Bool

unit :: Type
unit = Unit

arrow :: Type -> Type -> Type
arrow f s = f :-> s