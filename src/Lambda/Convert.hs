module Lambda.Convert (e2t, t2e, Context) where

import Data.List (elemIndex)

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term as T (Term(..))
import qualified Lambda.Expr as E (Expr(..), Ident)
import Data.Maybe ( fromJust )

type Context = [Maybe E.Ident]

e2t :: E.Expr -> T.Term
e2t = helper []
    where
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

t2e :: Term -> Expr
t2e = helper 0 []
    where
    helper :: Int -> Context -> Term -> Expr
    helper depth bound (T.Idx ident)
        | ident >= depth = error "free index in term"
        | otherwise      = E.Var $ fromJust $ bound !! ident
    helper depth bound (left :@: right) =
        let helper' = helper depth bound

            left' = helper' left
            right' = helper' right
        in left' :@ right'
    helper depth bound (T.Lmb name ty body) =
        let name' = name >>= (\ x -> return $ newName x bound)
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
