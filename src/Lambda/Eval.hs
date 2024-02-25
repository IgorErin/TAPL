module Lambda.Eval (
    shift, substDB, betaRuleDB,
    callByNameStep, normalOrderStep, appOrderStep, callByValueStep,
    eval
    ) where

import Lambda.Term (Term(..), Ident)

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
    helper m (Lmb info p) = Lmb info $ helper (succ m) p
    helper m (If guard ifTrue ifFalse) =
        let go = helper m

            guard' = go guard
            ifTrue' = go ifTrue
            ifFalse' = go ifFalse
        in If guard' ifTrue' ifFalse'
    helper _ Tru = Tru
    helper _ Fls = Fls

substDB :: Ident -> Term -> Term -> Term
substDB j n = helper
    where
    helper t@(Idx n')
        | j == n'   = n
        | otherwise = t
    helper (p :@: q) =
        let p' = helper p
            q' = helper q
        in p' :@: q'
    helper (Lmb info p) = Lmb info $ substDB (succ j) (shift 1 n) p
    helper (If guard ifTrue ifFalse) =
        let guard' = helper guard
            ifTrue' = helper ifTrue
            ifFalse' = helper ifFalse
        in If guard' ifTrue' ifFalse'
    helper Tru = Tru
    helper Fls = Fls

betaRuleDB :: Term -> Term
betaRuleDB ((Lmb _ t) :@: s) =
    let s' = shift 1 s
        t' = substDB 0 s' t
    in shift (-1) t'
betaRuleDB t = error $ "beta rule failed in " ++ show t

callByNameStep :: Term -> Maybe Term
callByNameStep (Idx _) = fail "Idx"
callByNameStep (Lmb _ _) = fail "Lmb"
callByNameStep r@(Lmb _ _ :@: _) = return $ betaRuleDB r
callByNameStep (t1 :@: t2) = do
    t1' <- callByNameStep t1
    return $ t1' :@: t2
callByNameStep _ = error "not implemented yet"

normalOrderStep :: Term -> Maybe Term
normalOrderStep (Idx _) = fail "Idx"
normalOrderStep (Lmb info t) = Lmb info <$> normalOrderStep t
normalOrderStep r@(Lmb _ _ :@: _) = return $ betaRuleDB r
normalOrderStep (t1 :@: t2) = case normalOrderStep t1 of
    Just t1' -> return $ t1' :@: t2
    Nothing -> (t1 :@:) <$> normalOrderStep t2
normalOrderStep _ = error "not implemented yet"

callByValueStep :: Term -> Maybe Term
callByValueStep (Idx _) = fail "Idx"
callByValueStep (Lmb _ _) = fail "Lmb"
callByValueStep r@(Lmb info body :@: arg) = case callByValueStep arg of
    Just arg' -> return (Lmb info body :@: arg')
    Nothing   -> return $ betaRuleDB r
callByValueStep (t1 :@: t2) = case callByValueStep t1 of
    Just t1' -> return $ t1' :@: t2
    Nothing  -> (t1 :@:) <$> callByValueStep t2
callByValueStep _ = error "not implemented yet"

appOrderStep :: Term -> Maybe Term
appOrderStep (Idx _) = fail "Idx"
appOrderStep (Lmb info body) = Lmb info <$> appOrderStep body
appOrderStep r@(Lmb info body :@: arg) = case appOrderStep arg of
    Just arg' -> return (Lmb info body :@: arg')
    Nothing   -> return $ betaRuleDB r
appOrderStep (t1 :@: t2) = case appOrderStep t1 of
    Just t1' -> return $ t1' :@: t2
    Nothing  -> (t1 :@:) <$> appOrderStep t2
appOrderStep _ = error "not implemented yet"

steps :: (a -> Maybe a) -> a -> [a]
steps f i = helper (Just i)
    where
    helper (Just item) = item : helper (f item)
    helper Nothing  = []

eval :: (a -> Maybe a) -> a -> a
eval f = last . steps f
