module Lambda.Infer (run) where

import Lambda.Term (Term(..))
import Lambda.Types (Type(..))

import Control.Monad (guard)

type Env = [Type]

run :: Term -> Maybe Type
run = infer' []
    where
    infer' :: Env -> Term -> Maybe Type
    infer' _ Tru = return Bool
    infer' _ Fls = return Bool
    infer' env (If cond ifTrue ifFalse) = do
        condT    <- infer' env cond
        ifTrueT  <- infer' env ifTrue
        ifFalseT <- infer' env ifFalse

        -- TODO add message
        guard (ifTrueT == ifFalseT)
        guard (condT == Bool)

        return ifTrueT
    infer' env (Idx v) = return $ env !! v
    infer' env (Lmb _ ty body) = do
        let env' = ty : env
        bodyT <- infer' env' body

        return $ ty :-> bodyT
    infer' env (t1 :@: t2) = do
        t1' <- infer' env t1
        t2' <- infer' env t2

        case t1' of
            argT :-> bodyT -> do
                guard (t2' == argT )

                return bodyT
            Bool -> fail "Expected fun type"
