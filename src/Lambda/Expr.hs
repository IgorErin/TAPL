module Lambda.Expr  (
    Expr(..), Symb,
    var, app, lam, lams,
) where

type Symb = String

infixl 4 :@

data Expr =
    Var Symb
    | Expr :@ Expr
    | Lam Symb Expr
    deriving (Eq, Read, Show)

var :: Symb -> Expr
var = Var

app :: Expr -> Expr -> Expr
app = (:@)

lam :: Symb -> Expr -> Expr
lam = Lam

lams :: Symb -> [Symb] -> Expr -> Expr
lams hd tl expr = lam hd $ helper tl
    where
    helper [] = expr
    helper (h : t) = lam h $ helper t
