module Lambda.Convert.ToTerm (run, Result) where

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr((:@)))

import qualified Lambda.Term  as T (Term(..))
import qualified Lambda.Expr  as E (Expr(..))
import qualified Lambda.Ident as Id (Label, Name)
import qualified Lambda.Index as Ind
import qualified Lambda.Info  as Inf (Info)
import qualified Lambda.Types as Ty (Type)
import qualified Lambda.Pattern as Pat(Pattern, Path, Getter(..), fetchAccess, check)

import qualified Lambda.Infer as I (run)

import Control.Monad.Reader
    (MonadReader(local), asks, ReaderT, runReaderT, lift)

type Context = Ind.Ctx (Maybe Id.Name)

type Result = Either Inf.Info T.Term

type HelperContext a = ReaderT Context (Either Inf.Info) a

run :: E.Expr -> Result
run e = runReaderT (helper e) Ind.emptyCtx
    where
    nameToIndex :: Id.Name -> HelperContext Ind.Index
    nameToIndex name = asks $ Ind.getIndex (Just name) name

    runRecordField :: (Id.Label, Expr) -> HelperContext (Id.Label, Term)
    runRecordField (lb, expr) = do
        term  <- helper expr

        return (lb, term)

    caseOfBranch :: Ty.Type -> Pat.Pattern -> Expr -> Expr
    caseOfBranch typ pat body =
        E.Lam (Just scurtini) typ $ foldr step body access
        where
        scurtini = "_scrutiny"

        transGet :: Pat.Path -> Expr -> Expr
        transGet p exr = foldr lam exr p
            where lam (Pat.Get f) acc = E.Get acc f

        step :: (Id.Name, Pat.Path) -> Expr -> Expr
        step (name, path) = E.Let name (transGet path (E.Var scurtini))

        access = Pat.fetchAccess pat

    helper :: E.Expr -> HelperContext T.Term
    helper (E.Var name) = T.Idx <$> nameToIndex name
    helper (left :@ right) = do
        left' <- helper left
        right' <- helper right

        return $ left' :@: right'
    helper (E.Lam name ty body) = do
        body' <- local (Ind.addData name) $ helper body

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
    helper (E.CaseOf escrut branchs) = do
        scrut <- helper escrut
        scrutTy <- lift $ I.run scrut

        let pats = fst <$> branchs
        lift $ Pat.check scrutTy pats

        let conts = uncurry (caseOfBranch scrutTy) <$> branchs
        lambs <- mapM helper conts

        let branchs' = zip pats lambs

        return $ T.CaseOf scrut branchs'
    helper (E.Record ls) = do
        -- TODO check record fields
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
    helper (E.Fix expr) = T.Fix <$> helper expr
    helper (E.Variant v) = T.Variant <$> mapM helper v
