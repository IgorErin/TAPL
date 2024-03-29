module Lambda.Convert.ToExpr (run) where

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term as T (Term(..))
import qualified Lambda.Expr as E (Expr(..))

import qualified Lambda.Ident as I

import Data.Maybe ( fromJust )

import Control.Monad.Reader (runReader, MonadReader (local, ask), Reader)

data BackContext = BContext { depth :: Int, ctx :: Context }

type Context = [Maybe I.Name]

newName :: I.Name -> Context -> I.Name
newName var ctx
    | Just var `elem` ctx = helper 0 var
    | otherwise      = var
    where
    helper :: Int -> I.Name -> I.Name
    helper count name
        | Just name' `elem` ctx = helper (succ count) name
        | otherwise        = name'
        where name' = name ++ show count

run :: Term -> Expr
run t = runReader (helper t) $ BContext { depth = 0, ctx = [] }
    where
    getName :: I.Index -> Reader BackContext I.Name
    getName ident = do
        BContext { depth, ctx } <- ask

        if ident >= depth
        then error "free index in term"
        else return $ fromJust $ ctx !! ident

    helper :: Term -> Reader BackContext Expr
    helper (T.Idx ident) = E.Var <$> getName ident
    helper (left :@: right) = do
        left' <- helper left
        right' <- helper right

        return $ left' :@ right'
    helper (T.Lmb name ty body) = do
        BContext { depth = _, ctx = c } <- ask

        let name' = flip newName c <$> name
        -- TODO rewrite
        body' <- local
            (\ BContext{ depth, ctx} ->
                 BContext { depth = succ depth, ctx = name' : ctx})
            $ helper body

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
