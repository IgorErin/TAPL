module Lambda.Pattern (
    Pattern(..),
    Path,
    Getter(..),
    fetchAccess, check,
    var, wild, record, variant) where

import Lambda.Ident (Label, Name)
import Lambda.Info (Info)
import qualified Lambda.Types as Ty (Type(..))

import Control.Monad.Except (MonadError (throwError))

import Data.List (find)

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

checkDup :: Pattern -> Either Info ()
checkDup pat = case findDup vars of
        Just name -> throwError $ "duplicate var in pattern:"+|name|+"."
        Nothing   -> return ()
    where
    getVars Wild = []
    getVars (Var n) = [n]
    getVars (Variant _ p) = getVars p
    getVars (Record ls) = concatMap (getVars . snd) ls

    vars = getVars pat

checkType :: Ty.Type -> Pattern -> Either Info ()
checkType _ Wild = return ()
checkType _ (Var _) = return ()
checkType (Ty.Record tls) (Record ls) = do
    mapM_ (\(lb, pat) -> case find ((== lb).fst) tls of
            Just (_, typ) -> checkType typ pat
            Nothing -> throwError $ "Pattern error. Field "+||lb||+"doesnot exists")
        ls
checkType (Ty.Variant fields) (Variant lb pat) = case find ((== lb).fst) fields of
        Just (_, typ) -> checkType typ pat
        Nothing -> throwError $ "Pattern error. No"+||lb||+"field"
checkType typ pat = throwError $ "Pattern error. Type check faild.\n"+||pat||+"\n"+||typ||+""

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
                Nothing -> undefined) ls'
    cover _ _ = False
checkOverlapping [] = undefined

checkRecordDup :: Pattern -> Either Info ()
checkRecordDup (Record ls) = case findDup lbs of
        Just fd -> throwError $ "duplicate field in pattern: "+||fd||+""
        Nothing -> mapM_ (checkRecordDup . snd) ls
    where lbs = fst <$> ls
checkRecordDup _ = return ()

check :: Ty.Type -> [Pattern] -> Either Info ()
check typ pats = do
    mapM_ checkDup pats
    mapM_ (checkType typ) pats
    mapM_ checkRecordDup pats

    checkOverlapping pats

------------------------ constructors ------------------------

var :: Name -> Pattern
var = Var

wild :: Pattern
wild = Wild

record :: [(Label, Pattern)] -> Pattern
record = Record

variant :: Label -> Pattern -> Pattern
variant = Variant