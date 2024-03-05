module Lambda.Infer (run, Result) where

import Lambda.Term (Term(..))
import Lambda.Types (Type(..))

import Control.Monad.Except (MonadError(throwError))

import Data.Text

type Env = [Type]
type Info = Text

type Result = Either Info Type

---------------- Check helpers -----------------

checkEqType :: Type -> Type -> Info -> Either Info ()
checkEqType f s i
    | f == s    = return ()
    | otherwise = throwError i

checkBool :: Type -> Info -> Either Info ()
checkBool = checkEqType Bool

splitArrow :: Type -> Info -> Either Info (Type, Type)
splitArrow t i = case t of
    argT :-> bodyT -> return (argT, bodyT)
    _              -> throwError i

------------------ Infer ---------------------------

run :: Term -> Either Info Type
run = infer' []
    where
    infer' :: Env -> Term -> Either Info Type
    infer' _ Tru = return Bool
    infer' _ Fls = return Bool
    infer' env (If cond ifTrue ifFalse) = do
        condT    <- infer' env cond
        checkBool condT $ "Cond type should be Bool"

        ifTrueT  <- infer' env ifTrue
        ifFalseT <- infer' env ifFalse
        checkEqType ifTrueT ifFalseT "Branch types must be equal"

        return ifTrueT
    infer' env (Idx v) = return $ env !! v
    infer' env (Lmb _ ty body) = do
        let env' = ty : env
        bodyT <- infer' env' body

        return $ ty :-> bodyT
    infer' env (t1 :@: t2) = do
        t1T <- infer' env t1
        t2T <- infer' env t2

        (argT, bodyT) <- splitArrow t1T "Arrow expected in application"

        checkEqType t2T argT "Arg type mismatch"
        return bodyT