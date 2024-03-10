module Lambda.Infer (run, Info, Result) where

import Lambda.Term    as Te (Term(..), Ident)
import Lambda.Types   as Ty (Type(..))

import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Reader

import Data.Text (Text)
import Fmt ( (+|), (+||), (|+), (||+) )

type Env = [Ty.Type]

type Info = Text

---------------- Check helpers -----------------

checkEqType :: Ty.Type -> Ty.Type -> Info -> Infer ()
checkEqType f s i
    | f == s    = return ()
    | otherwise = throwError i

checkBool :: Ty.Type -> Info -> Infer ()
checkBool = checkEqType Bool

splitArrow :: Ty.Type -> Info -> Infer (Ty.Type, Ty.Type)
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

type Infer a = ReaderT Env (Either Info) a

type Result = Either Info Type

run :: Term -> Result
run t = runReaderT (infer' t) []
    where
    typeofIdent :: Te.Ident -> Infer Type
    typeofIdent ident = asks (!! ident)

    infer' ::Term -> Infer Type
    infer' Tru = return Bool
    infer' Fls = return Bool
    infer' Te.Unit = return Ty.Unit
    infer' (If cond ifTrue ifFalse) = do
        condT    <- infer' cond
        checkBool condT $ mkCondIsBoolInfo condT

        ifTrueT  <- infer' ifTrue
        ifFalseT <- infer' ifFalse
        checkEqType ifTrueT ifFalseT $ mkBranchInfo ifTrueT ifFalseT

        return ifTrueT
    infer' (Idx v) = typeofIdent v
    infer' (Lmb _ ty body) = do
        bodyT <- local (ty:) $ infer' body

        return $ ty :-> bodyT
    infer' (t1 :@: t2) = do
        t1T <- infer' t1
        t2T <- infer' t2

        (argT, bodyT) <- splitArrow t1T $ mkSplitArrowInfo t2T t1T

        let errorInfo = mkTypeEqInfo (Just "Arg type mismatch") t2T argT
        checkEqType t2T argT errorInfo
        return bodyT