module Lambda.Expr.Tree (
    Expr_(..),
    Tree,
    pattern (:>),
    Fix(..),
    Compose(..),
    Binder,
    Record,
    Field
    ) where

import Lambda.Types (Type)
import Lambda.Ident (Name, Label)
import Lambda.Oper (UnOp)
import Lambda.Pattern (Pattern)

type Binder = Maybe Name

infixl 4 :@

newtype Fix f = Fix { unFix :: f (Fix f) }

newtype Compose f g x = Compose { getCompose :: f (g x) }

type Tree f a = Fix (Compose ((,) a) f)

pattern (:>) :: a -> f (Tree f a) -> Tree f a
pattern a :> f = Fix (Compose (a, f))

{-# COMPLETE (:>) #-}

type Field expr = (Label, expr)
type Record expr = [Field expr]

data Expr_ self =
    Var Name
    | Tru
    | Fls
    | Unit
    | Int Int
    | UnOp UnOp self
    | If self self self
    | self :@ self
    | Lam Binder Type self
    | Let Name self self
    | CaseOf self [(Pattern, self)]
    | Record (Record self)
    | Variant (Field self)
    | Get self Label
    | EFix self