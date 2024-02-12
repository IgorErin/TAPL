module Arith (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Arith.Ast
import Arith.Eval as E (step, eval)

stepCases :: [(Term, Maybe Term)]
stepCases = [
    (true, Nothing),
    (if_ true true false, Just true),
    (if_ false true false, Just false),
    (succ_ $ succ_ $ if_ true zero false, Just $ succ_ $ succ_ zero),
    (pred_ $ succ_ $ if_ true zero false, Just $ pred_ $ succ_ zero)]

stepTests :: TestTree
stepTests = mkTests "step" E.step stepCases

evalCases :: [(Term, Term)]
evalCases = [
    (true, true),
    (if_ true true false, true),
    (if_ false true false, false),
    (succ_ $ succ_ $ if_ true zero false, succ_ $ succ_ zero),
    (pred_ $ succ_ $ if_ true zero false, zero),
    (pred_ $ true, pred_ true)]

evalTests :: TestTree
evalTests = mkTests "eval" E.eval evalCases

tests :: TestTree
tests = testGroup "Arith eval" [stepTests, evalTests]

---------------- Helpers -----------------------

mkTests :: (Show a, Eq a) => String -> (Term -> a) -> [(Term, a)]-> TestTree
mkTests name testFun cases =
    testGroup name $
    (\ (str, expected) ->
        let x = testFun str in
        testCase (show str) (x @?= expected))
        <$>
        cases
