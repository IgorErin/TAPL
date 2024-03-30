module Lambda.Eval (
    shift, substDB, betaRuleDB,
    callByValueStep,
    steps, eval
    ) where

import Lambda.Term (Term(..))
import Lambda.Ident (Index)
import Lambda.Oper (BinOp(..))
import qualified Lambda.Pattern as Pat (Pattern(..))

import Data.List (find)

-- TODO duplication shift and substDB

shift :: Int -> Term -> Term
shift k = helper 0
    where
    helper m (Idx n)
        | n < m = Idx n
        | otherwise = Idx $ n + k
    helper m (p :@: q) =
        let p' = helper m p
            q' = helper m q
        in  p' :@: q'
    helper m (Lmb t info p) = Lmb t info $ helper (succ m) p
    helper m (If guard ifTrue ifFalse) =
        let go = helper m

            guard' = go guard
            ifTrue' = go ifTrue
            ifFalse' = go ifFalse
        in If guard' ifTrue' ifFalse'
    helper _ Tru = Tru
    helper _ Fls = Fls
    helper _ Unit = Unit
    helper m (Record ls) = Record $ (helper m <$>) <$> ls
    helper m (Get t lb)  = Get (helper m t) lb
    helper _ n@(Int _)    = n
    helper m (BinOp left op right) =
        let left' = helper m left
            right' = helper m right
        in BinOp left' op right'
    helper m (Fix term) = Fix $ helper m term
    helper m (Variant v) = Variant $ helper m <$> v
    helper m (CaseOf scrut branches) =
        let scrut' = helper m scrut
            branches' = (helper m <$>) <$> branches
        in CaseOf scrut' branches'

substDB :: Index -> Term -> Term -> Term
substDB j n = helper
    where
    helper t@(Idx n')
        | j == n'   = n
        | otherwise = t
    helper (p :@: q) =
        let p' = helper p
            q' = helper q
        in p' :@: q'
    helper (Lmb t info p) = Lmb t info $ substDB (succ j) (shift 1 n) p
    helper (If guard ifTrue ifFalse) =
        let guard' = helper guard
            ifTrue' = helper ifTrue
            ifFalse' = helper ifFalse
        in If guard' ifTrue' ifFalse'
    helper Tru = Tru
    helper Fls = Fls
    helper Unit = Unit
    helper (Record ls) =  Record $ (helper <$>) <$> ls
    helper (Get t lb) = Get (helper t) lb
    helper t@(Int _) = t
    helper (BinOp left op right) =
        let left' = helper left
            right' = helper right
        in BinOp left' op right'
    helper (Fix term) = Fix $ helper term
    helper (Variant v) = Variant $ helper <$> v
    helper (CaseOf scrut branches) =
        let scrut' = helper scrut
            branches' = (helper <$>) <$> branches
        in CaseOf scrut' branches'

betaRuleDB :: Term -> Term
betaRuleDB ((Lmb _ _ t) :@: s) =
    let s' = shift 1 s
        t' = substDB 0 s' t
    in shift (-1) t'
betaRuleDB t = error $ "beta rule failed in " ++ show t

-- Rework : isValue and Nothing seems to do one jobe two times. duplication
isValue :: Term -> Bool
isValue Lmb {}      = True
isValue Tru         = True
isValue Fls         = True
isValue (Record ls) = all (isValue . snd) ls
isValue Unit        = True
isValue (Int _)     = True
isValue (Variant v) = isValue $ snd v
isValue _           = False

termOfBool :: Bool -> Term
termOfBool True = Tru
termOfBool False = Fls

runBinOp :: Term -> BinOp -> Term -> Term
runBinOp left Eq right  = termOfBool (left == right)
runBinOp left NEq right = termOfBool (left /= right)
runBinOp (Int left) Add (Int right) = Int $ left + right
runBinOp (Int left) Sub (Int right) = Int $ left - right
runBinOp (Int left) Mul (Int right) = Int $ left * right
runBinOp (Int left) Lt (Int right)  = termOfBool (left < right)
runBinOp (Int left) Le (Int right)  = termOfBool (left <= right)
runBinOp (Int left) Gt (Int right)  = termOfBool (left > right)
runBinOp (Int left) Ge (Int right)  = termOfBool (left >= right)
runBinOp _ _ _ = error "unexpected argument in binOp"

isBranch :: Term -> Pat.Pattern -> Bool
isBranch (Record _) (Pat.Record _) = True
isBranch (Variant (lb, term)) (Pat.Variant lb' pat') =
    lb == lb' && isBranch term pat'
isBranch _ _ = False -- TODO

callByValueStep :: Term -> Maybe Term
callByValueStep (If Tru ifTrue _) = return ifTrue
callByValueStep (If Fls _ ifFalse) = return ifFalse
callByValueStep (If cond ifTrue ifFalse) = do
    cond' <- callByValueStep cond
    return $ If cond' ifTrue ifFalse
callByValueStep (t1 :@: t2) | not $ isValue t1 = do
    t1' <- callByValueStep t1
    return $ t1' :@: t2
callByValueStep (t1 :@: t2) | isValue t1 && not (isValue t2) = do
    t2' <- callByValueStep t2
    return $ t1 :@: t2'
callByValueStep r@(Lmb {} :@: value) | isValue value =
    return $ betaRuleDB r
-- Record
callByValueStep (Record ls) =
    let run (hd@(lb, term) : tl)
            | isValue term = (hd:) <$> run tl
            | otherwise    =  do
                term' <- callByValueStep term

                return $ (lb, term') : tl
        run []             =  Nothing
    in Record <$> run ls
-- Get
callByValueStep (Get t lb) | not $ isValue t = do
    t' <- callByValueStep t

    return $ Get t' lb
callByValueStep (Get r@(Record ls) lb) | isValue r =
    case find ((== lb) . fst) ls of
        Just t -> return $ snd t
        Nothing -> error ""
callByValueStep (Get v@(Variant (lb', f)) lb) | isValue v =
    if lb == lb'
    then return f
    else error "Incorrect getter" -- TODO more info
callByValueStep (Get t lb) =
    error $ "Attemt to get" ++ show lb ++ "on" ++ show t
callByValueStep (CaseOf scrut brances) | not $ isValue scrut = do
    scrut' <-callByValueStep scrut

    return $ CaseOf scrut' brances
callByValueStep (CaseOf scrut branches) = do
    case find (isBranch scrut . fst) branches of
        Just (_, cont) -> return $ cont :@: scrut
        Nothing -> error "cannot find branch"
-- Variant
callByValueStep t@(Variant _) | isValue t = fail "Value variant"
callByValueStep (Variant f) = Variant <$> mapM callByValueStep f
-- BinOp
callByValueStep (BinOp left op right)
    | not $ isValue left = do
        left' <- callByValueStep left
        return $ BinOp left' op right
    | isValue left && not (isValue right) = do
        right' <- callByValueStep right
        return $ BinOp left op right'
    | otherwise = return $ runBinOp left op right
-- Fix
callByValueStep point@(Fix term)
    | not $ isValue term = Fix <$> callByValueStep term
    | otherwise          = return $ betaRuleDB $ term :@: point
-- Stack
callByValueStep (Idx _) = fail "Idx"
callByValueStep (Lmb {}) = fail "Lmb"
callByValueStep Tru = fail "Tru"
callByValueStep Fls = fail "Fls"
callByValueStep Unit = fail "Unit"
callByValueStep (_ :@: _) = fail "App"
callByValueStep (Int _) = fail "Int"


steps :: (a -> Maybe a) -> a -> [a]
steps f i = helper (Just i)
    where
    helper (Just item) = item : helper (f item)
    helper Nothing  = []

eval :: (a -> Maybe a) -> a -> a
eval f = last . steps f
