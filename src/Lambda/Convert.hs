module Lambda.Convert (e2t, t2e, Context) where

import Data.List (elemIndex)

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term as T (Term(..), Ident)
import qualified Lambda.Expr as E (Expr(..), Ident)
import Data.Maybe ( fromJust )

import Control.Monad.Reader

type Context = [Maybe E.Ident]

e2t :: E.Expr -> T.Term
e2t e = runReader (helper e) []
    where
    nameToIndex :: E.Ident -> Reader Context T.Ident
    nameToIndex name = do
        ls <- ask

        case elemIndex (Just name) ls of
            Just number -> return number
            Nothing     -> error $ "undefined reference: " ++ name

    helper :: E.Expr -> Reader Context T.Term
    helper (E.Var name) = T.Idx <$> nameToIndex name
    helper (left :@ right) = do
        left' <- helper left
        right' <- helper right

        return $ left' :@: right'
    helper (E.Lam name ty body) = do
        body' <- local (name:) $ helper body

        return $ T.Lmb name ty body'
    helper (E.If guard etrue efalse) = do
        guard' <- helper guard
        etrue' <- helper etrue
        efalse' <- helper efalse

        return $ T.If guard' etrue' efalse'
    helper E.Tru = return T.Tru
    helper E.Fls = return T.Fls
    helper E.Unit = return T.Unit
    helper (E.Let {}) = error "Let case"

data BackContext = BContext { depth :: Int, ctx :: Context }

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
t2e t = runReader (helper t) $ BContext { depth = 0, ctx = [] }
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
