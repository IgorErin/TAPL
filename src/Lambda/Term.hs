module Lambda.Term (Term(..), Ident) where

type Ident = Int
type Info = String

infixl 4 :@:

data Term =
    Idx Ident
    | Term :@: Term
    | Lmb Info Term
    deriving (Eq, Read, Show)