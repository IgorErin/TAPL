module Lambda.Convert.ToExpr (run) where

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term as T (Term(..), Ident)
import qualified Lambda.Expr as E (Expr(..), Ident)

import Data.Maybe ( fromJust )

import Control.Monad.Reader (runReader, MonadReader (local, ask), Reader)

data BackContext = BContext { depth :: Int, ctx :: Context }

type Context = [Maybe E.Ident]

newName :: E.Ident -> Context -> E.Ident
newName var ctx
    | Just var `elem` ctx = helper 0 var
    | otherwise      = var
    where
    helper :: Int -> E.Ident -> E.Ident
    helper count name
        | Just name' `elem` ctx = helper (succ count) name
        | otherwise        = name'
        where name' = name ++ show count

run :: Term -> Expr
run t = runReader (helper t) $ BContext { depth = 0, ctx = [] }
    where
    getName :: T.Ident -> Reader BackContext E.Ident
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
