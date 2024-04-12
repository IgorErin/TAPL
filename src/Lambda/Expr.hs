{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda.Expr  (
    Expr_(..),
    Expr,
    TypedExpr,
    Binder,
    toText,
    var, app, lam, lams,
    if_, false, true,
    unit,
    ascription,
    let_, letrec,
    record,
    variant,
    get,
    int,
    unOp,
    caseOf,
    pattern (:>)
) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Fmt

import Lambda.Types (Type, arrow)
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
    | Record Record
    | Variant Field
    | Get self Label
    | EFix self

type Expr = Tree Expr_ ()

toText :: Expr -> Builder
toText (() :> Var name) = ""+|name|+""
toText (() :> Tru) = "true"
toText (() :> Fls) = "false"
toText (() :> Unit) = "unit"
toText (() :> Int n) = ""+|n|+""
toText (() :> UnOp op e) = ""+||op||+" "+|toText e|+""
toText (() :> If g t f) = "if "+| toText g|+"\nthen "+| toText t|+"\nelse "+| toText f|+""
toText (() :> (left :@ right)) = ""+|toText left|+" "+| toText right |+""
toText (() :> (Lam binder t e)) = "(fun ("+||binder||+"): "+||t||+" -> "+|toText e|+")"
toText (() :> (Let name e b)) = "let "+|name|+" = "+|toText e|+" in\n"+| toText b|+""
toText (() :> CaseOf s ls) = "case "+|toText s|+" of\n"+|caseText ls|+""
    where
    caseText :: [(Pattern, Expr)] -> Builder
    caseText ((pat, e) : tl) = ""+||pat||+" -> "+| toText e|+"\n"+| caseText tl|+""
    caseText [] = ""
toText (() :> Record r) = "Record { "+||fields r||+"}"
    where
    fields :: [Field] -> Builder
    fields ((l, e) : tl) = ""+||l|+" : "+| toText e|+", "+| fields tl|+""
    fields [] = ""
toText (() :> Variant (lb, e)) = "< "+||lb||+" : "+| toText e|+" >"
toText (() :> (Get e lb)) = "("+| toText e|+")."+|lb|+""
toText (() :> EFix e) = "fix "+| toText e |+""
toText (Fix { unFix = Compose { getCompose = ((), e) }}) = toText $ mk e

instance Show Expr where
    show :: Expr -> String
    show = fmt . toText

type TypedExpr = Tree Expr_ Type

type Field = (Label, Expr)
type Record = [Field]

mk :: Expr_ Expr -> Expr
mk  = (:>) ()

true :: Expr
true = mk Tru

false :: Expr
false = mk Fls

if_ :: Expr -> Expr -> Expr -> Expr
if_ g t f = mk $ If g t f

var :: Name -> Expr
var = mk . Var

app :: Expr -> Expr -> Expr
app l r = mk $ l :@ r

lam :: Binder -> Type -> Expr -> Expr
lam b t e = mk $ Lam b t e

lams :: NonEmpty (Binder, Type) -> Expr -> Expr
lams ((ident, ty) :| tl) expr = lam ident ty $ mkLam expr tl

unit :: Expr
unit = mk Unit

ascription :: Expr -> Type -> Expr
ascription e t = lam (Just "x") t (var "x") `app` e

record :: Record -> Expr
record = mk . Record

variant :: Field -> Expr
variant = mk . Variant

get :: Expr -> Label -> Expr
get e l = mk $ Get e l

int :: Int -> Expr
int = mk . Int

caseOf :: Expr -> [(Pattern, Expr)] -> Expr
caseOf e ls = mk $ CaseOf e ls

unOp :: UnOp -> Expr -> Expr
unOp op e = mk $ UnOp op e

--------------------------- Let ----------------------------

mkLam :: Expr -> [(Binder, Type)] -> Expr
mkLam expr ((bin, ty) : tl) = lam bin ty $ mkLam expr tl
mkLam expr [] = expr

let_ :: Name -> [(Binder, Type)] -> Expr -> Expr -> Expr
let_ name params expr body =
    mk $
    Let name (mkLam expr params) body

letrec :: Name -> [(Binder, Type)] -> Type -> Expr -> Expr -> Expr
letrec name params resultType expr body =
    let argTypes = snd <$> params
        funType = Prelude.foldr arrow resultType argTypes
        fix = mk $ EFix (lam (Just name) funType (mkLam expr params))
    in mk $ Let name fix body
