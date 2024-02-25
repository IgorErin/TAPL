module Lambda.Convert (e2t, t2e, Context) where

import Data.List (elemIndex)

import Lambda.Term (Term(..))
import Lambda.Expr (Expr(..), Symb)

type Context = [Symb]

e2t :: Expr -> (Context,Term)
e2t term = (freeVars, helper freeVars term)
    where
    freeVars = reverse $ fetchFree term

    helper :: Context -> Expr -> Term
    helper ctx (Var name) = case elemIndex name ctx of
        Just number -> Idx number
        Nothing     -> error $ "undefined reference: " ++ name
    helper ctx (left :@ right) =
        let left' = helper ctx left
            right' = helper ctx right
        in left' :@: right'
    helper ctx (Lam name body) =
        Lmb name $ helper (name : ctx) body

addVar :: Symb -> Context -> Context
addVar symb ctx =
    if symb `elem` ctx
    then ctx
    else symb : ctx

fetchFree :: Expr -> Context
fetchFree = helper [] []
    where
    helper :: Context -> Context -> Expr -> Context
    helper bound free (Var name) =
        if name `elem` bound
        then free
        else addVar name free
    helper bound free (left :@ right) =
        let free' = helper bound free left
        in helper bound free' right
    helper bound free (Lam name body) =
        let bound' = name : bound
        in helper bound' free body

newName :: Symb -> Context -> Symb
newName var ctx
    | var `elem` ctx = helper 0 var
    | otherwise      = var
    where
    helper :: Int -> Symb -> Symb
    helper count name
        | name' `elem` ctx = helper (succ count) name
        | otherwise        = name'
        where name' = name ++ show count

t2e :: Context -> Term -> Expr
t2e ctx = helper 0 []
    where
    helper :: Int -> Context -> Term -> Expr
    helper depth bound (Idx ident)
        | ident >= depth = Var $ ctx !! (ident - length bound)
        | otherwise      = Var $ bound !! ident
    helper depth bound (left :@: right) =
        let left' = helper depth bound left
            right' = helper depth bound right
        in left' :@ right'
    helper depth bound (Lmb name body) =
        let name' = newName name (bound ++ ctx)
            bound' = name' : bound
            depth' = succ depth
        in Lam name' $ helper depth' bound' body

