module Lambda.Oper (UnOp(..),
    succ_, pred_, isZero
    ) where

data UnOp = Succ | Pred | IsZero deriving (Show, Eq)

succ_ :: UnOp
succ_ = Succ

pred_ :: UnOp
pred_ = Pred

isZero :: UnOp
isZero = IsZero
