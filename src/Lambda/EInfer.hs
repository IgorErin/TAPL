module Lambda.EInfer (run, Result) where

import Lambda.Expr as Le (Expr_(..), Expr, toText, TypedExpr, pattern (:>))

import Lambda.Info as Inf (Info)
import Lambda.Types (Type)
import qualified Lambda.Types as Ty
    (Record, Type(..), fields, fmember, fget, arrow)
import qualified Lambda.Pattern as Pat (check, getVarTypes)
import Lambda.Oper (UnOp(..) )
import Lambda.Ident (Name)

import qualified Data.Map as Map (empty, union, insert, lookup, Map)
import Data.List (find)

import Fmt ((+||), (||+))

import Control.Monad.Except ( MonadError(throwError))
import Control.Monad (unless)
import Control.Monad.Reader (ReaderT (runReaderT), lift, ask, local)

type Ctx = Map.Map Name Type
type Infer a = ReaderT Ctx (Either Info) a

type Result = Either Info Type

ctxLocal :: Name -> Type -> Infer a -> Infer a
ctxLocal name ty = local $ Map.insert name ty

typeOfUnOp :: UnOp -> Type
typeOfUnOp Succ = Ty.Int `Ty.arrow` Ty.Int
typeOfUnOp Pred = Ty.Int `Ty.arrow` Ty.Int
typeOfUnOp IsZero = Ty.Int `Ty.arrow` Ty.Bool

app :: Type -> Type -> Infer Type
app f s = do
    (base, result) <- arrow f

    unless (base == s) $ throwError $ "Arg type mismatch: "+||base||+" <> "+||s||+""

    return result

arrow :: Type -> Infer (Type, Type)
arrow (base Ty.:-> result) = return (base, result)
arrow t = do throwError $ "arrow expected, but got: "+||t||+""

branch :: [Type] -> Infer Type
branch (hd : tl) = case find (/= hd) tl of
    Just t -> throwError $ "Types of branch should be equal. "+||t||+" <> "+||hd||+""
    Nothing -> return hd
branch [] = throwError "Empty branch"

bool :: Type -> Infer Type
bool Ty.Bool = return Ty.Bool
bool t = throwError $ "Bool type expected, but: "+||t||+""

nameLookup :: Ctx -> Name -> Infer Type
nameLookup ctx name = case Map.lookup name ctx of
        Just x -> return x
        Nothing -> throwError $ "Unbound name: "+||name||+""

infer :: Expr -> Infer Type
infer (() :> Var v) = do
    ctx <- ask

    nameLookup ctx v
infer (() :> Tru) = return Ty.Bool
infer (() :> Fls) = return Ty.Bool
infer (() :> Unit) = return Ty.Unit
infer (() :> Int _) = return Ty.Int
infer (() :> UnOp op expr) = do
    let opT = typeOfUnOp op
    exprT <- infer expr

    app opT exprT
infer (() :> If grd true false) = do
    _ <- bool <$> infer grd

    branch =<< mapM infer [true, false]
infer (() :> (lmb :@ () :> Variant (lb, expr))) = do
    lmbT <- infer lmb
    (base, result) <- arrow lmbT
    exprT <- infer expr

    let filedT = (lb, exprT)

    case base of
        Ty.Variant tfl
            | Ty.fmember filedT tfl -> return ()
            | otherwise -> throwError $ "no "+||(lb, exprT)||+"filed in "+||lmbT||+""
        t -> throwError $ "expected Variant, but: "+||t||+""

    return result
infer v@(() :> Variant _) = do
    throwError $ "Variant typing failed. More information needed: "+||toText v||+""
infer (() :> (left :@ right)) = do
    left' <- infer left
    right' <- infer right

    app left' right'
infer (() :> Lam binder typ body) = do
    bodyT <- case binder of
        Just name -> do ctxLocal name typ $ infer body
        Nothing -> infer body

    return $ typ Ty.:-> bodyT
infer (() :> Let name expr body) = do
    nameT <- infer expr
    ctxLocal name nameT $ infer body
infer (() :> CaseOf scr branchs) = do
    scrT <- infer scr

    let pats = fst <$> branchs
    _ <- lift $ Pat.check scrT pats

    btypes <- mapM (\(pat, expr) -> do
        patVarTypes <- lift $ Pat.getVarTypes scrT pat
        local (Map.union patVarTypes) $ infer expr)
        branchs

    branch btypes
infer (() :> Record ls) = do
    x <- Ty.fields <$> mapM (mapM infer) ls

    return $ Ty.Record x
infer (() :> Get expr tag) = do
    exprT <- infer expr

    let findFiled :: Ty.Record -> Infer Type
        findFiled ls = maybe
            (throwError $ "record filed not found: "+||tag||+"")
            return
            (Ty.fget tag ls)

    case exprT of
        Ty.Record ls -> findFiled ls
        Ty.Variant ls -> findFiled ls
        t -> throwError $ "Getter of "+||t||+""
infer (() :> EFix expr) = do
    exprT <- infer expr
    (baseT, resultT) <- arrow exprT

    if baseT == resultT
    then return baseT
    else throwError $
        "Types in fix abs must be equal: "+||baseT||+" <> "+||resultT||+""
infer _ = undefined

run :: Le.Expr ->  Result
run e = runReaderT (infer e) Map.empty
