module Lambda.Convert.ToTerm (run, Result) where

import Data.List (elemIndex)

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term as T (Term(..), Ident)
import qualified Lambda.Expr as E (Expr(..), Ident)

import qualified Lambda.Infer as I (run, Info)

import Control.Monad.Reader
    (MonadReader(local, ask), ReaderT, runReaderT, lift)

type Context = [Maybe E.Ident]

type Result = Either I.Info T.Term

type HelperContext a = ReaderT Context (Either I.Info) a

run :: E.Expr -> Result
run e = runReaderT (helper e) []
    where
    nameToIndex :: E.Ident -> HelperContext T.Ident
    nameToIndex name = do
        ls <- ask

        case elemIndex (Just name) ls of
            Just number -> return number
            Nothing     -> error $ "undefined reference: " ++ name

    helper :: E.Expr -> HelperContext T.Term
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
    helper (E.Let name expr body) = do
        expr' <- helper expr
        ty <- lift $ I.run expr'

        let lam = E.Lam (Just name) ty body

        helper $ lam :@ expr