module Lambda.Expr  (
    Expr(..), Ident, Binder,
    var, app, lam, lams,
    if_, false, true,
    unit,
    ascription
) where

import Data.List.NonEmpty ( NonEmpty(..) )

import Lambda.Types (Type)

type Ident = String

type Binder = Maybe Ident

infixl 4 :@

data Expr =
    Var Ident
    | Tru
    | Fls
    | Unit
    | If Expr Expr Expr
    | Expr :@ Expr
    | Lam Binder Type Expr
    deriving (Eq, Read, Show)

true :: Expr
true = Tru

false :: Expr
false = Fls

if_ :: Expr -> Expr -> Expr -> Expr
if_ = If

var :: Ident -> Expr
var = Var

app :: Expr -> Expr -> Expr
app = (:@)

lam :: Binder -> Type -> Expr -> Expr
lam = Lam

lams :: NonEmpty (Binder, Type) -> Expr -> Expr
lams ((ident, ty) :| tl) expr = lam ident ty $ helper tl
    where
    helper [] = expr
    helper ((id', ty') : tl') = lam id' ty' $ helper tl'

unit :: Expr
unit = Unit

ascription :: Expr -> Type -> Expr
ascription e t = Lam (Just "x") t (Var "x") :@ e
