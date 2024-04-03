{-# LANGUAGE InstanceSigs #-}
module Lambda.Types (
    Type(..),
    Fields(..),
    Field,
    fields,
    fmember,
    fget,
    Record,
    bool, unit, int,
    arrow,
    record,
    variant
) where

import Lambda.Ident (Label)

import Data.List (find)

infixr 3 :->

type Field = (Label, Type)
newtype Fields = Fields [Field] deriving (Show)

fields :: [Field] -> Fields
fields = Fields

instance Ord Fields where
    (<=) :: Fields -> Fields -> Bool
    (<=) (Fields l1) (Fields l2) = all (`elem` l2) l1

instance Eq Fields where
    (==) :: Fields -> Fields -> Bool
    (==) l1 l2 = l1 <= l2 && l2 <= l1

fmember :: Field -> Fields -> Bool
fmember f (Fields fs) = f `elem` fs

fget :: Label -> Fields -> Maybe Type
fget lb (Fields fls) = snd <$> find ((== lb). fst) fls

type Record = Fields
type Variant = Fields

data Type =
    Bool
    | Unit
    | Int
    | Type :-> Type
    | Record Record
    | Variant Variant
    deriving (Show)

instance Eq Type where
    (==) :: Type -> Type -> Bool
    (==) Bool Bool = True
    (==) Unit Unit = True
    (==) Int Int = True
    (==) (b1 :-> r1) (b2 :-> r2) =
        b1 == b2 && r1 == r2
    (==) (Record ls1) (Record ls2) = ls1 == ls2
    (==) (Variant ls1) (Variant ls2) = ls1 == ls2
    (==) _ _ = False

bool :: Type
bool = Bool

unit :: Type
unit = Unit

int :: Type
int = Int

arrow :: Type -> Type -> Type
arrow f s = f :-> s

record :: [Field] -> Type
record = Record . fields

variant :: [Field] -> Type
variant = Variant . fields