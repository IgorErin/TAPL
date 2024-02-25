module Lambda.Term (Term(..), Type(..), Ident) where

type Ident = Int
type Info = String

infixl 4 :@:
infixr 3 :->

data Type =
    Bool
    | Type :-> Type
    deriving (Read, Show, Eq)

data Term =
    Idx Ident
    | Tru
    | Fls
    | If Term Term Term
    | Term :@: Term
    | Lmb Info Type Term
    deriving (Eq, Read, Show)