module Lambda.Pattern (
    Pattern(..),
    Path,
    Getter(..),
    fetchAccess,
    check, getVarTypes,
    var, wild, record, variant) where

import Lambda.Ident (Label, Name)
import Lambda.Info (Info)
import qualified Lambda.Types as Ty (Type(..), fget)

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State

import Data.List (find)
import Data.Map as Map

import Fmt ((+|), (|+), (||+), (+||))

newtype Getter = Get Label

type Path = [Getter]

data Pattern =
    Wild
    | Var Name
    | Variant Label Pattern
    | Record [(Label, Pattern)]
    deriving (Eq, Show)

findDup :: Eq a => [a] -> Maybe a
findDup (hd : tl)
    | hd `elem` tl = Just hd
    | otherwise = findDup tl
findDup [] = Nothing

fetchAccess :: Pattern -> [(Name, Path)]
fetchAccess Wild = []
fetchAccess (Var n) = [(n, [])]
fetchAccess (Variant lb pat) = ((Get lb:) <$>) <$> fetchAccess pat
fetchAccess (Record ls) = concatMap (\(lb, pat) -> fetchAccess (Variant lb pat)) ls

checkDupVar :: Pattern -> Either Info ()
checkDupVar pat = case findDup vars of
        Just name -> throwError $ "duplicate var in pattern:"+|name|+"."
        Nothing   -> return ()
    where
    getVars Wild = []
    getVars (Var n) = [n]
    getVars (Variant _ p) = getVars p
    getVars (Record ls) = concatMap (getVars . snd) ls

    vars = getVars pat

type TypeCtx = Map.Map Name Ty.Type
type TypeCtxM a = StateT TypeCtx (Either Info) a

getVarTypes :: Ty.Type -> Pattern -> Either Info TypeCtx
getVarTypes t p = execStateT (helper p t) Map.empty
    where
    helper :: Pattern -> Ty.Type -> TypeCtxM ()
    helper Wild _ = return ()
    helper (Var name) typ = modify $ Map.insert name typ
    helper (Record ls) (Ty.Record tls) = do
        mapM_ (\(lb, pat) -> case Ty.fget lb tls of
            Just typ -> helper pat typ
            Nothing -> throwError $ "Pattern error. Field "+||lb||+"doesnot exists") ls
    helper (Variant lb pat) (Ty.Variant tls) = case Ty.fget lb tls of
        Just typ -> helper pat typ
        Nothing -> throwError $ "Pattern error. No"+||lb||+"field"
    helper typ pat = throwError $ "Pattern error. Type check faild.\n"+||pat||+"\n"+||typ||+""

checkOverlapping :: [Pattern] -> Either Info ()
checkOverlapping (hd : tl) = case unreachable of
        Just pat -> throwError $ "Unreachable pattern: "+||pat||+""
        Nothing -> return ()
    where
    unreachable = find (hd `cover`) tl

    cover :: Pattern -> Pattern -> Bool
    cover Wild _ = True
    cover (Var _) _ = True
    cover (Variant tag pat) (Variant tag' pat') =
        tag == tag' && pat `cover` pat'
    cover (Record ls) (Record ls') = all
        (\(lb, pat) -> case find ((== lb) . fst) ls of
                Just (_, pat') -> cover pat pat'
                Nothing -> False) ls'
    cover _ _ = False
checkOverlapping [] = return ()

checkRecordDup :: Pattern -> Either Info ()
checkRecordDup (Record ls) = case findDup lbs of
        Just fd -> throwError $ "duplicate field in pattern: "+||fd||+""
        Nothing -> mapM_ (checkRecordDup . snd) ls
    where lbs = fst <$> ls
checkRecordDup _ = return ()

checkExhaustive :: Pattern -> Either Info ()
checkExhaustive = undefined -- TODO

check :: Ty.Type -> [Pattern] -> Either Info ()
check ty pats = do
    mapM_ checkDupVar pats
    mapM_ checkRecordDup pats

    checkOverlapping pats
    mapM_ (getVarTypes ty) pats

------------------------ constructors ------------------------

var :: Name -> Pattern
var = Var

wild :: Pattern
wild = Wild

record :: [(Label, Pattern)] -> Pattern
record = Record

variant :: Label -> Pattern -> Pattern
variant = Variant