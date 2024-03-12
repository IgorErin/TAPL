module Lambda.Convert.ToTerm (run, Result) where

import Data.List (elemIndex)

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term as T (Term(..))
import qualified Lambda.Expr as E (Expr(..))
import qualified Lambda.Ident as Id (Label, Name, Index)

import qualified Lambda.Infer as I (run, Info)

import Control.Monad.Reader
    (MonadReader(local, ask), ReaderT, runReaderT, lift)

type Context = [Maybe Id.Name]

type Result = Either I.Info T.Term

type HelperContext a = ReaderT Context (Either I.Info) a

run :: E.Expr -> Result
run e = runReaderT (helper e) []
    where
    nameToIndex :: Id.Name -> HelperContext Id.Index
    nameToIndex name = do
        ls <- ask

        case elemIndex (Just name) ls of
            Just number -> return number
            Nothing     -> error $ "undefined reference: " ++ name

    runRecordField :: (Id.Label, Expr) -> HelperContext (Id.Label, Term)
    runRecordField (lb, expr) = do
        term  <- helper expr

        return (lb, term)

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
    helper (E.Record ls) = do
        ls' <- mapM runRecordField ls

        return $ T.Record ls'
    helper (E.Get expr lb) = do
        term <- helper expr

        return $ T.Get term lb
    helper (E.Int n) = return $ T.Int n
    helper (E.BinOp left op right) = do
        left' <- helper left
        right' <- helper right

        return $ T.BinOp left' op right'
