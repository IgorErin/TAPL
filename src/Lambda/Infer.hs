module Lambda.Infer (run, Info, Result) where

import Lambda.Term    as Te (Term(..), Field)
import Lambda.Types   as Ty (Type(..), Field, Record)
import Lambda.Ident   as I  (Index, Label)
import Lambda.Oper    as Op (BinOp(..))

import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Reader
    ( ReaderT(runReaderT), asks, MonadReader(local) )

import Data.List (find)

import Data.Text (Text)
import Fmt ( (+|), (+||), (|+), (||+) )

type Env = [Ty.Type]

type Info = Text

---------------- Check helpers -----------------

checkEqType :: Ty.Type -> Ty.Type -> Info -> Infer ()
checkEqType (Ty.Record ll) (Ty.Record rl) i =
    let containse :: Ty.Record -> Ty.Record -> Bool
        containse (hd : tl) l2 = hd `elem` l2 && containse tl l2
        containse []        _  = True
    in if containse ll rl && containse rl ll
       then return ()
       else throwError i
checkEqType f s i
    | f == s    = return ()
    | otherwise = throwError i

checkBool :: Ty.Type -> Info -> Infer ()
checkBool = checkEqType Bool

splitArrow :: Ty.Type -> Info -> Infer (Ty.Type, Ty.Type)
splitArrow t i = case t of
    argT :-> bodyT -> return (argT, bodyT)
    _              -> throwError i

checkGetType :: Ty.Type -> I.Label -> Infer Ty.Type
checkGetType ty@(Ty.Record ls) lb = case find ((== lb) . fst) ls of
        Just x -> return $ snd x
        Nothing -> throwError $ "Attemt to get "+||lb||+"on type:"+||ty||+". No such field."
checkGetType ty lb = throwError $ "Attemt to get "+||lb||+"on type:"+||ty||+"."

checkBinOpType :: Ty.Type -> BinOp -> Ty.Type -> Infer Ty.Type
checkBinOpType Ty.Int op Ty.Int = return $ forInt op
    where
    forInt :: BinOp -> Type
    forInt Add = Ty.Int
    forInt Sub = Ty.Int
    forInt Mul = Ty.Int
    forInt Le  = Ty.Bool
    forInt Lt = Ty.Bool
    forInt Ge = Ty.Bool
    forInt Gt = Ty.Bool
    forInt Eq  = Ty.Bool
    forInt NEq = Ty.Bool
checkBinOpType leftt op rightt | isGeneric op = do
    checkEqType leftt rightt "types of bin op must be equal"

    return Bool
    where
    isGeneric :: BinOp -> Bool
    isGeneric Eq  = True
    isGeneric NEq = True
    isGeneric _   = False
checkBinOpType _ _ _ = error "unmatched case in binop"

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
run t = runReaderT (typeOf t) []
    where
    typeofIdent :: I.Index -> Infer Type
    typeofIdent ident = asks (!! ident)

    typeOfField :: Te.Field -> Infer Ty.Field
    typeOfField  (lb, term) = do
        tt <- typeOf term

        return (lb, tt)

    typeOf :: Term -> Infer Type
    typeOf Tru = return Bool
    typeOf Fls = return Bool
    typeOf Te.Unit = return Ty.Unit
    typeOf (If cond ifTrue ifFalse) = do
        condT    <- typeOf cond
        checkBool condT $ mkCondIsBoolInfo condT

        ifTrueT  <- typeOf ifTrue
        ifFalseT <- typeOf ifFalse
        checkEqType ifTrueT ifFalseT $ mkBranchInfo ifTrueT ifFalseT

        return ifTrueT
    typeOf (Idx v) = typeofIdent v
    typeOf (Lmb _ ty body) = do
        bodyT <- local (ty:) $ typeOf body

        return $ ty :-> bodyT
    typeOf (t1 :@: t2) = do
        t1T <- typeOf t1
        t2T <- typeOf t2

        (argT, bodyT) <- splitArrow t1T $ mkSplitArrowInfo t2T t1T

        let errorInfo = mkTypeEqInfo (Just "Arg type mismatch") t2T argT
        checkEqType t2T argT errorInfo
        return bodyT
    typeOf (Te.Record ls) = Ty.Record <$> mapM typeOfField ls
    typeOf (Te.Get term lb) = do
        termT <- typeOf term

        checkGetType termT lb
    typeOf (Te.Int _) = return Ty.Int
    typeOf (Te.BinOp left op right) = do
        left' <- typeOf left
        right' <- typeOf right

        checkBinOpType left' op right'
    typeOf (Te.Fix term) = do
        termT <- typeOf term

        (base, result) <- splitArrow termT "Fix splittig"

        checkEqType base result
            $ "Fix types must be equal: "+||base||+" <> "+||result||+""

        return result

