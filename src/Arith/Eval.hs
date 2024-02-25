module Arith.Eval (step, eval) where

import Arith.Ast (Term(..))

isNum :: Term -> Bool
isNum (TSucc term) = isNum term
isNum (TPred term) = isNum term
isNum TZero        = True
isNum _            = False

step :: Term -> Maybe Term
step (TIf TTrue true _) = return true
step (TIf TFalse _ false) = return false
step (TIf cond true false) = do
    cond' <- step cond
    return $ TIf cond' true false
step (TSucc term) = do
    term' <- step term
    return $ TSucc term'
step (TPred TZero) = return TZero
step (TPred (TSucc term)) | isNum term =
    return term
step (TPred term) = do
    term' <- step term
    return $ TPred term'
step (TIsZero (TSucc term)) | isNum term =
    return TFalse
step (TIsZero TZero) = return TTrue
step (TIsZero term) = do
    term' <- step term
    return $ TIsZero term'
step term = fail $ show term

iterateWhileJust :: a -> (a -> Maybe a) -> [a]
iterateWhileJust start next = loop $ Just start
    where
        loop (Just x) = x : loop (next x)
        loop Nothing  = []

steps :: Term  -> [Term]
steps start = iterateWhileJust start step

eval :: Term -> Term
eval = last . steps
