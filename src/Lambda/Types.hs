module Lambda.Types (
    Type(..),
    bool,
    arrow
) where

infixr 3 :->

data Type =
    Bool
    | Type :-> Type
    deriving (Read, Show, Eq)

bool :: Type
bool = Bool

arrow :: Type -> Type -> Type
arrow f s = f :-> s