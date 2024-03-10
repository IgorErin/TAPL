module Lambda.Expr  (
    Expr(..), Binder,
    var, app, lam, lams,
    if_, false, true,
    unit,
    ascription,
    let_,
    record
) where

import Data.List.NonEmpty ( NonEmpty(..) )

import Lambda.Types (Type)
import Lambda.Ident (Name, Label)

type Binder = Maybe Name

infixl 4 :@

data Expr =
    Var Name
    | Tru
    | Fls
    | Unit
    | If Expr Expr Expr
    | Expr :@ Expr
    | Lam Binder Type Expr
    | Let Name Expr Expr
    | Record [(Label, Expr)]
    deriving (Eq, Show)

true :: Expr
true = Tru

false :: Expr
false = Fls

if_ :: Expr -> Expr -> Expr -> Expr
if_ = If

var :: Name -> Expr
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

let_ :: Name -> Expr -> Expr -> Expr
let_ = Let

record :: [(Name, Expr)] -> Expr
record = Record
