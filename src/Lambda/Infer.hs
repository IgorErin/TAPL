module Lambda.Infer (run, Result) where

import Lambda.Term    as Te (Term(..))
import Lambda.Types   as Ty (Type(..))

import Control.Monad.Except (MonadError(throwError))

import Data.Text
import Fmt

type Env = [Ty.Type]
type Info = Text

type Result = Either Info Ty.Type

---------------- Check helpers -----------------

checkEqType :: Ty.Type -> Ty.Type -> Info -> Either Info ()
checkEqType f s i
    | f == s    = return ()
    | otherwise = throwError i

checkBool :: Ty.Type -> Info -> Either Info ()
checkBool = checkEqType Bool

splitArrow :: Ty.Type -> Info -> Either Info (Ty.Type, Ty.Type)
splitArrow t i = case t of
    argT :-> bodyT -> return (argT, bodyT)
    _              -> throwError i

------------------ Message helpers -----------------

mkBranchInfo :: Type -> Type -> Info
mkBranchInfo = mkTypeEqInfo $ Just "In If branchase"

mkCondIsBoolInfo :: Type -> Info
mkCondIsBoolInfo t =
    "The condition must be of Bool type. Actual: "+||t||+""

mkSplitArrowInfo :: Type -> Type -> Info
mkSplitArrowInfo argT actual =
    "Expected function type like: "+||argT||+" -> _. Actual: "+||actual||+""

mkTypeEqInfo :: Maybe Text -> Type -> Type -> Info
mkTypeEqInfo (Just i) firstT secondT =
    ""+|i|+" types must be same.\n "+||firstT||+" <> "+||secondT||+"."
mkTypeEqInfo Nothing firstT secondT =
    "Types must be the same in If branches.\n "+||firstT||+" <> "+||secondT||+"."

------------------ Infer ---------------------------

run :: Term -> Either Info Type
run = infer' []
    where
    infer' :: Env -> Term -> Either Info Type
    infer' _ Tru = return Bool
    infer' _ Fls = return Bool
    infer' _ Te.Unit = return Ty.Unit
    infer' env (If cond ifTrue ifFalse) = do
        condT    <- infer' env cond
        checkBool condT $ mkCondIsBoolInfo condT

        ifTrueT  <- infer' env ifTrue
        ifFalseT <- infer' env ifFalse
        checkEqType ifTrueT ifFalseT $ mkBranchInfo ifTrueT ifFalseT

        return ifTrueT
    infer' env (Idx v) = return $ env !! v
    infer' env (Lmb _ ty body) = do
        let env' = ty : env
        bodyT <- infer' env' body

        return $ ty :-> bodyT
    infer' env (t1 :@: t2) = do
        t1T <- infer' env t1
        t2T <- infer' env t2

        (argT, bodyT) <- splitArrow t1T $ mkSplitArrowInfo t2T t1T

        checkEqType t2T argT $ mkTypeEqInfo (Just "Arg type mismatch") t2T argT
        return bodyT