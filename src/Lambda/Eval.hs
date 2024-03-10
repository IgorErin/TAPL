module Lambda.Eval (
    shift, substDB, betaRuleDB,
    callByValueStep,
    steps, eval
    ) where

import Lambda.Term (Term(..))
import Lambda.Ident

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
    helper _ (Record {}) = error "Record"

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
    helper (Record {}) = error "record"

betaRuleDB :: Term -> Term
betaRuleDB ((Lmb _ _ t) :@: s) =
    let s' = shift 1 s
        t' = substDB 0 s' t
    in shift (-1) t'
betaRuleDB t = error $ "beta rule failed in " ++ show t

isValue :: Term -> Bool
isValue Lmb {} = True
isValue Tru       = True
isValue Fls       = True
isValue _         = False

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
callByValueStep (Idx _) = fail "Idx"
callByValueStep (Lmb {}) = fail "Lmb"
callByValueStep Tru = fail "Tru"
callByValueStep Fls = fail "Fls"
callByValueStep Unit = fail "Unit"
callByValueStep (_ :@: _) = fail "App"
callByValueStep (Record {}) = error "record"

steps :: (a -> Maybe a) -> a -> [a]
steps f i = helper (Just i)
    where
    helper (Just item) = item : helper (f item)
    helper Nothing  = []

eval :: (a -> Maybe a) -> a -> a
eval f = last . steps f
