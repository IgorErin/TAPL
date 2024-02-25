module Lambda.Convert (e2t, t2e, Context) where

import Data.List (elemIndex)

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term as T (Term(..), Type(..))
import qualified Lambda.Expr as E (Expr(..), Symb)

type Context = [E.Symb]

e2t :: E.Expr -> (Context, T.Term)
e2t term = (freeVars, helper freeVars term)
    where
    freeVars = reverse $ fetchFree term

    helper :: Context -> E.Expr -> T.Term
    helper ctx (E.Var name) = case elemIndex name ctx of
        Just number -> T.Idx number
        Nothing     -> error $ "undefined reference: " ++ name
    helper ctx (left :@ right) =
        let left' = helper ctx left
            right' = helper ctx right
        in left' :@: right'
    helper ctx (E.Lam name body) =
        -- TODO add types to parser
        T.Lmb name T.Bool $ helper (name : ctx) body
    helper ctx (E.If guard etrue efalse) =
        let guard' = helper ctx guard
            etrue' = helper ctx etrue
            efalse' = helper ctx efalse
        in T.If guard' etrue' efalse'
    helper _ E.Tru = T.Tru
    helper _ E.Fls = T.Fls

addVar :: E.Symb -> Context -> Context
addVar symb ctx =
    if symb `elem` ctx
    then ctx
    else symb : ctx

fetchFree :: E.Expr -> Context
fetchFree = helper [] []
    where
    helper :: Context -> Context -> Expr -> Context
    helper bound free (E.Var name) =
        if name `elem` bound
        then free
        else addVar name free
    helper bound free (left :@ right) =
        let free' = helper bound free left
        in helper bound free' right
    helper bound free (E.Lam name body) =
        let bound' = name : bound
        in helper bound' free body
    helper bound free (E.If guard etrue efalse) =
        let free'   = helper bound free   guard
            free''  = helper bound free'  etrue
            free''' = helper bound free'' efalse
        in free'''
    helper _ free E.Tru = free
    helper _ free E.Fls = free

newName :: E.Symb -> Context -> E.Symb
newName var ctx
    | var `elem` ctx = helper 0 var
    | otherwise      = var
    where
    helper :: Int -> E.Symb -> E.Symb
    helper count name
        | name' `elem` ctx = helper (succ count) name
        | otherwise        = name'
        where name' = name ++ show count

t2e :: Context -> Term -> Expr
t2e ctx = helper 0 []
    where
    helper :: Int -> Context -> Term -> Expr
    helper depth bound (T.Idx ident)
        | ident >= depth = E.Var $ ctx !! (ident - length bound)
        | otherwise      = E.Var $ bound !! ident
    helper depth bound (left :@: right) =
        let helper' = helper depth bound

            left' = helper' left
            right' = helper' right
        in left' :@ right'
    helper depth bound (T.Lmb name _ body) =
        let name' = newName name (bound ++ ctx)
            bound' = name' : bound
            depth' = succ depth
        in E.Lam name' $ helper depth' bound' body
    helper depth bound (T.If guard ttrue tfalse) =
        let helper' = helper depth bound

            guard' = helper' guard
            ttrue' = helper' ttrue
            tfalse' = helper' tfalse
        in E.If guard' ttrue' tfalse'
    helper _ _ T.Tru = E.Tru
    helper _ _ T.Fls = E.Fls
