module Lambda.Eval (
    shift, substDB, betaRuleDB,
    callByValueStep,
    steps, eval
    ) where

import Lambda.Term (Term(..))
import Lambda.Ident (Index)

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
isValue _           = False

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
callByValueStep (Get r@(Record ls) lb) | isValue r =
    case find ((== lb) . fst) ls of
        Just t -> return $ snd t
        Nothing -> error ""
callByValueStep (Get t lb) | not $ isValue t = do
    t' <- callByValueStep t

    return $ Get t' lb
callByValueStep (Get t lb) =
    error $ "Attemt to get" ++ show lb ++ "on" ++ show t
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
