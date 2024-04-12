module Lambda.Convert.ToTerm (run, Result) where

import Lambda.Term (Term((:@:)))
import Lambda.Expr (Expr_(..), Expr, pattern (:>))

import qualified Lambda.Term  as T (Term(..))
import qualified Lambda.Expr  as E (lam, get, let_, var, app, Expr)
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
        E.lam (Just scurtini) typ $ foldr step body access
        where
        scurtini = "_scrutiny"

        transGet :: Pat.Path -> Expr -> Expr
        transGet p exr = foldr lam exr p
            where lam (Pat.Get f) acc = E.get acc f

        step :: (Id.Name, Pat.Path) -> E.Expr -> E.Expr
        step (name, path) = E.let_ name [] (transGet path (E.var scurtini))

        access = Pat.fetchAccess pat

    helper :: E.Expr -> HelperContext T.Term
    helper (() :> Var name) = T.Idx <$> nameToIndex name
    helper (() :> (left :@ right)) = do
        left' <- helper left
        right' <- helper right

        return $ left' :@: right'
    helper (() :> Lam name _ body) = do
        body' <- local (Ind.addData name) $ helper body

        return $ T.Lmb name body'
    helper (() :> If guard etrue efalse) = do
        guard' <- helper guard
        etrue' <- helper etrue
        efalse' <- helper efalse

        return $ T.If guard' etrue' efalse'
    helper (() :> Tru) = return T.Tru
    helper (() :> Fls) = return T.Fls
    helper (() :> Unit) = return T.Unit
    helper (() :> Let name expr body) = do
        expr' <- helper expr
        ty <- lift $ I.run expr'

        let lam = E.lam (Just name) ty body

        helper $ lam `E.app` expr
    helper (() :> CaseOf escrut branchs) = do
        scrut <- helper escrut
        scrutTy <- lift $ I.run scrut

        let pats = fst <$> branchs
        lift $ Pat.check scrutTy pats

        let conts = uncurry (caseOfBranch scrutTy) <$> branchs
        lambs <- mapM helper conts

        let branchs' = zip pats lambs

        return $ T.CaseOf scrut branchs'
    helper (() :> Record ls) = do
        -- TODO check record fields
        ls' <- mapM runRecordField ls

        return $ T.Record ls'
    helper (() :> Get expr lb) = do
        term <- helper expr

        return $ T.Get term lb
    helper (() :> Int n) = return $ T.Int n
    helper (() :> UnOp op expr) = do
        expr' <- helper expr

        return $ T.UnOp op expr'
    helper (() :> EFix expr) = T.Fix <$> helper expr
    helper (() :> Variant v) = T.Variant <$> mapM helper v
    helper _ = error "TODO Pattern for fix"


