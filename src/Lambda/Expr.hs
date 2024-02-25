module Lambda.Expr  (
    Expr(..), Ident,
    var, app, lam, lams,
    if_, false, true
) where

import Data.List.NonEmpty

import Lambda.Types (Type)

type Ident = String

infixl 4 :@

data Expr =
    Var Ident
    | Tru
    | Fls
    | If Expr Expr Expr
    | Expr :@ Expr
    | Lam Ident Type Expr
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

lam :: Ident -> Type -> Expr -> Expr
lam = Lam

lams :: NonEmpty (Ident, Type) -> Expr -> Expr
lams ((ident, ty) :| tl) expr = lam ident ty $ helper tl
    where
    helper [] = expr
    helper ((id', ty') : tl') = lam id' ty' $ helper tl'
