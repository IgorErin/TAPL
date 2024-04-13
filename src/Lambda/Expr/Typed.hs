{-# LANGUAGE InstanceSigs #-}

module Lambda.Expr.Typed (run, Result, ShowTyped(..)) where

import Lambda.Expr.Tree (Expr_(..), Tree, pattern (:>))
import Lambda.Expr.Raw (Expr, toText)

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

type Result = Either Info TypedExpr

ctxLocal :: Name -> Type -> Infer a -> Infer a
ctxLocal name ty = local $ Map.insert name ty

typeOfUnOp :: UnOp -> Type
typeOfUnOp Succ = Ty.Int `Ty.arrow` Ty.Int
typeOfUnOp Pred = Ty.Int `Ty.arrow` Ty.Int
typeOfUnOp IsZero = Ty.Int `Ty.arrow` Ty.Bool

type TypedExpr = Tree Expr_ Type

--------------------- Checks -------------------------

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

---------------- Smart constructors ----------------

infer :: Expr -> Infer TypedExpr
infer (() :> Var v) = do
    ctx <- ask
    t <- nameLookup ctx v

    return $ t :> Var v
infer (() :> Tru) = return $ Ty.Bool :> Tru
infer (() :> Fls) = return $ Ty.Bool :> Fls
infer (() :> Unit) = return $ Ty.Unit :> Unit
infer (() :> Int n) = return $ Ty.Int :> Int n
infer (() :> UnOp op expr) = do
    let opT = typeOfUnOp op

    expr' <- infer expr
    let (exprT :> _) = expr'

    t' <- app opT exprT

    return (t' :> UnOp op expr')
infer (() :> If grd true false) = do
    grd' <- infer grd
    let (grdT :> _) = grd'

    _ <- bool grdT

    true' <- infer true
    let (trueT :> _) = true'

    false' <- infer false
    let (falseT :> _) = false'

    resultT <- branch [trueT, falseT]

    let resultExpr = If grd' true' false'
    return $ resultT :> resultExpr
infer (() :> (lmb :@ () :> Variant (lb, expr))) = do
    lmb' <- infer lmb
    let (lmbT :> _) = lmb'

    (base, result) <- arrow lmbT

    expr' <- infer expr
    let (exprT :> _) = expr'

    case base of
        Ty.Variant tfl
            | Ty.fmember (lb, exprT) tfl -> return ()
            | otherwise -> throwError $ "no "+||(lb, exprT)||+"filed in "+||lmbT||+""
        t -> throwError $ "expected Variant, but: "+||t||+""

    return $ result :> (lmb' :@ expr')
infer v@(() :> Variant _) = do
    throwError $ "Variant typing failed. More information needed: "+||toText v||+""
infer (() :> (left :@ right)) = do
    left' <- infer left
    let (leftT :> _) = left'

    right' <- infer right
    let (rightT :> _) = right'

    resultT <- app leftT rightT

    return $ resultT :> (left' :@ right')
infer (() :> Lam binder typ body) = do
    body' <- case binder of
        Just name -> do ctxLocal name typ $ infer body
        Nothing -> infer body

    let (bodyT :> _) = body'
    let t = typ Ty.:-> bodyT

    return $ t :> Lam binder typ body'
infer (() :> Let name expr body) = do
    expr' <- infer expr
    let (nameT :> _) = expr'

    body' <- ctxLocal name nameT $ infer body
    let (bodyT :> _) = body'

    return $ bodyT :> Let name expr' body'
infer (() :> CaseOf scr branchs) = do
    scr' <- infer scr
    let (scrT :> _) = scr'

    let pats = fst <$> branchs
    _ <- lift $ Pat.check scrT pats

    branchs' <- mapM (\(pat, expr) -> do
        patVarTypes <- lift $ Pat.getVarTypes scrT pat
        expr' <- local (Map.union patVarTypes) $ infer expr

        return (pat, expr'))
        branchs

    resultT <- branch $ (\(_, t :> _) -> t) <$> branchs' -- TODO

    return $ resultT :> CaseOf scr' branchs'
infer (() :> Record ls) = do
    record <- mapM (mapM infer) ls
    let recordT = Ty.fields $ (\(lb, t :> _) -> (lb, t)) <$> record

    return $ Ty.Record recordT :> Record record
infer (() :> Get expr tag) = do
    expr' <- infer expr
    let (exprT :> _) = expr'

    let findFiled :: Ty.Record -> Infer Type
        findFiled ls = maybe
            (throwError $ "record filed not found: "+||tag||+"")
            return
            (Ty.fget tag ls)

    resultT <- case exprT of
        Ty.Record ls -> findFiled ls
        Ty.Variant ls -> findFiled ls
        t -> throwError $ "Getter of "+||t||+""

    return $ resultT :> Get expr' tag
infer (() :> EFix expr) = do
    expr' <- infer expr
    let (exprT :> _) = expr'

    (baseT, resultT) <- arrow exprT

    ty <- if baseT == resultT
          then return baseT
          else throwError $
                "Types in fix abs must be equal: "+||baseT||+" <> "+||resultT||+""

    return $ ty :> EFix expr'

run :: Expr ->  Result
run e = runReaderT (infer e) Map.empty

----------------------- show ------------------------

newtype ShowTyped = ShowTyped TypedExpr

instance Show ShowTyped where
    show :: ShowTyped -> String
    show (ShowTyped (t :> _)) = show t