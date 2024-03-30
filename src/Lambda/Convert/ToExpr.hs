module Lambda.Convert.ToExpr (run) where

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term as T (Term(..))
import qualified Lambda.Expr as E (Expr(..))

import Lambda.Ident ( Name )
import qualified Lambda.Index as Ind

import Data.Maybe ( fromJust )

import Control.Monad.Reader (runReader, asks, ask, MonadReader (local), Reader)

type Ctx = Ind.Ctx (Maybe Name)

newName :: Name -> Ctx -> Name
newName var ctx
    | Just var `Ind.exist` ctx = helper 0 var
    | otherwise      = var
    where
    helper :: Int -> Name -> Name
    helper count name
        | Just name' `Ind.exist` ctx = helper (succ count) name
        | otherwise        = name'
        where name' = name ++ show count

run :: Term -> Expr
run t = runReader (helper t) Ind.emptyCtx
    where
    getName :: Ind.Index -> Reader Ctx Name
    getName index = asks $ fromJust . Ind.backData index

    helper :: Term -> Reader (Ind.Ctx (Maybe Name)) Expr
    helper (T.Idx ident) = E.Var <$> getName ident
    helper (left :@: right) = do
        left' <- helper left
        right' <- helper right

        return $ left' :@ right'
    helper (T.Lmb name ty body) = do
        ctx <- ask

        let name' = flip newName ctx <$> name

        body' <- local (Ind.addData name') $ helper body

        return $ E.Lam name' ty body'
    helper (T.If guard ttrue tfalse) = do
        guard' <- helper guard
        ttrue' <- helper ttrue
        tfalse' <- helper tfalse

        return $ E.If guard' ttrue' tfalse'
    helper T.Tru = return E.Tru
    helper T.Fls = return E.Fls
    helper T.Unit = return E.Unit
    helper (T.Record ls) = do
        ls' <- mapM (mapM helper) ls

        return $ E.Record ls'
    helper (T.Get term lb) = do
        term' <- helper term

        return $ E.Get term' lb
    helper (T.Int n) = return $ E.Int n
    helper (T.BinOp left op right) = do
        left' <- helper left
        right' <- helper right

        return $ E.BinOp left' op right'
    helper (T.Fix term) = E.Fix <$> helper term
    helper (T.Variant v) = E.Variant <$> mapM helper v
    helper (T.CaseOf scrut branches) = do
        scrut' <- helper scrut
        branches' <- mapM (mapM helper) branches

        return $ E.CaseOf scrut' branches'