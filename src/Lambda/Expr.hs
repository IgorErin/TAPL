module Lambda.Expr  (
    Expr(..), Binder,
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
    caseOf
) where

import Data.List.NonEmpty ( NonEmpty(..) )

import Lambda.Types (Type, arrow)
import Lambda.Ident (Name, Label)
import Lambda.Oper (UnOp)
import Lambda.Pattern (Pattern)

type Binder = Maybe Name

infixl 4 :@

type Field = (Label, Expr)
type Record = [Field]

data Expr =
    Var Name
    | Tru
    | Fls
    | Unit
    | Int Int
    | UnOp UnOp Expr
    | If Expr Expr Expr
    | Expr :@ Expr
    | Lam Binder Type Expr
    | Let Name Expr Expr
    | CaseOf Expr [(Pattern, Expr)]
    | Record Record
    | Variant Field
    | Get Expr Label
    | Fix Expr
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
lams ((ident, ty) :| tl) expr = lam ident ty $ mkLam expr tl

unit :: Expr
unit = Unit

ascription :: Expr -> Type -> Expr
ascription e t = Lam (Just "x") t (Var "x") :@ e

record :: Record -> Expr
record = Record

variant :: Field -> Expr
variant = Variant

get :: Expr -> Label -> Expr
get = Get

int :: Int -> Expr
int = Int

caseOf :: Expr -> [(Pattern, Expr)] -> Expr
caseOf = CaseOf

unOp :: UnOp -> Expr -> Expr
unOp = UnOp

--------------------------- Let ----------------------------

mkLam :: Expr -> [(Binder, Type)] -> Expr
mkLam expr ((bin, ty) : tl) = Lam bin ty $ mkLam expr tl
mkLam expr [] = expr

let_ :: Name -> [(Binder, Type)] -> Expr -> Expr -> Expr
let_ name params expr = Let name (mkLam expr params)

letrec :: Name -> [(Binder, Type)] -> Type -> Expr -> Expr -> Expr
letrec name params resultType expr body =
    let argTypes = snd <$> params
        funType = foldr arrow resultType argTypes
        fix = Fix (Lam (Just name) funType (mkLam expr params))
    in Let name fix body


