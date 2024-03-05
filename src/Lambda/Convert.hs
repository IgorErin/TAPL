module Lambda.Convert (e2t, t2e, Context) where

import Data.List (elemIndex)

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term as T (Term(..))
import qualified Lambda.Expr as E (Expr(..), Ident)
import Data.Maybe

type Context = [Maybe E.Ident]

e2t :: E.Expr -> (Context, T.Term)
e2t term = (freeVars, helper freeVars term)
    where
    freeVars = reverse $ fetchFree term

    helper :: Context -> E.Expr -> T.Term
    helper ctx (E.Var name) = case elemIndex (Just name) ctx of
        Just number -> T.Idx number
        Nothing     -> error $ "undefined reference: " ++ name
    helper ctx (left :@ right) =
        let left' = helper ctx left
            right' = helper ctx right
        in left' :@: right'
    helper ctx (E.Lam name ty body) =
        T.Lmb name ty $ helper (name : ctx) body
    helper ctx (E.If guard etrue efalse) =
        let guard' = helper ctx guard
            etrue' = helper ctx etrue
            efalse' = helper ctx efalse
        in T.If guard' etrue' efalse'
    helper _ E.Tru = T.Tru
    helper _ E.Fls = T.Fls
    helper _ E.Unit = T.Unit

addVar :: E.Ident -> Context -> Context
addVar symb ctx =
    if Just symb `elem` ctx
    then ctx
    else Just symb : ctx

fetchFree :: E.Expr -> Context
fetchFree = helper [] []
    where
    helper :: Context -> Context -> Expr -> Context
    helper bound free (E.Var name) =
        if Just name `elem` bound
        then free
        else addVar name free
    helper bound free (left :@ right) =
        let free' = helper bound free left
        in helper bound free' right
    helper bound free (E.Lam name _ body) =
        let bound' = name : bound
        in helper bound' free body
    helper bound free (E.If guard etrue efalse) =
        let free'   = helper bound free   guard
            free''  = helper bound free'  etrue
            free''' = helper bound free'' efalse
        in free'''
    helper _ free E.Tru = free
    helper _ free E.Fls = free
    helper _ free E.Unit = free

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

t2e :: Context -> Term -> Expr
t2e ctx = helper 0 []
    where
    helper :: Int -> Context -> Term -> Expr
    helper depth bound (T.Idx ident)
        | ident >= depth = E.Var $ fromJust $ ctx !! (ident - length bound)
        | otherwise      = E.Var $ fromJust $ bound !! ident
    helper depth bound (left :@: right) =
        let helper' = helper depth bound

            left' = helper' left
            right' = helper' right
        in left' :@ right'
    helper depth bound (T.Lmb name ty body) =
        let name' = name >>= (\ x -> return $ newName x (bound ++ ctx))
            bound' = name' : bound
            depth' = succ depth
        in E.Lam name' ty $ helper depth' bound' body
    helper depth bound (T.If guard ttrue tfalse) =
        let helper' = helper depth bound

            guard' = helper' guard
            ttrue' = helper' ttrue
            tfalse' = helper' tfalse
        in E.If guard' ttrue' tfalse'
    helper _ _ T.Tru = E.Tru
    helper _ _ T.Fls = E.Fls
    helper _ _ T.Unit = E.Unit
