module Lambda.Oper (BinOp(..),
    add, sub, mul, gt, ge, lt, le, eq, neq
    ) where

data BinOp = Add | Sub | Mul | Lt | Le | Gt | Ge | Eq | NEq deriving (Eq, Show)

add :: BinOp
add = Add

sub :: BinOp
sub = Sub

mul :: BinOp
mul = Mul

gt :: BinOp
gt = Gt

ge :: BinOp
ge = Ge

lt :: BinOp
lt = Lt

le :: BinOp
le = Le

eq :: BinOp
eq = Eq

neq :: BinOp
neq = NEq
