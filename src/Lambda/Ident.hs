module Lambda.Ident (
    Name,
    Index(..), intOfIndex,
    Label) where

type Name = String

newtype Index = MkIndex (Int, String) deriving (Show, Eq)

intOfIndex :: Index -> Int
intOfIndex (MkIndex (n, _)) = n

type Label = String

