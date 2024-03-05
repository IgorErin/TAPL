module Tests.Lambda.All(tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Lambda.Common as TestLambda (runShow, runString)

import qualified Lambda.Convert     as LC (e2t, t2e)

import qualified Lambda.Expr        as LE (Expr)
import qualified Lambda.Term        as LT (Term)

import qualified Lambda.Lexer       as LL (alexScanTokens, Token)
import qualified Lambda.Parser      as LR (run)
import qualified Lambda.Eval        as LEval (callByValueStep, steps, eval)
import qualified Lambda.Infer       as Infer (run, Result)

tests :: IO TestTree
tests = testGroup "Lambda" <$>
    sequence [
        lexerTests,
        parserTests,
        staticDistanceTests,
        backToNamesTests,
        callByValueEvalStepsTests,
        callByValueEvalTests,
        inferTests
    ]

lexer :: String -> [LL.Token]
lexer = LL.alexScanTokens

lexerTests :: IO TestTree
lexerTests = TestLambda.runShow "Lexer" lexer

parser :: String -> LE.Expr
parser = LR.run . lexer

parserTests :: IO TestTree
parserTests = TestLambda.runShow "Parser" parser

staticDistance :: String -> LT.Term
staticDistance = LC.e2t . parser

staticDistanceTests :: IO TestTree
staticDistanceTests = TestLambda.runShow "StaticDistance" staticDistance

backToNames :: String -> LE.Expr
backToNames str =
    let term = staticDistance str
    in LC.t2e term

backToNamesTests :: IO TestTree
backToNamesTests = TestLambda.runShow "BackToNames" backToNames

callByValueEvalSteps :: String -> String
callByValueEvalSteps str =
    let term = staticDistance str
        termSteps = LEval.steps LEval.callByValueStep term
        str' = unlines $ map show termSteps
    in str'

callByValueEvalStepsTests :: IO TestTree
callByValueEvalStepsTests  = TestLambda.runString "byValueSteps" callByValueEvalSteps

callByValueEval :: String -> LT.Term
callByValueEval str =
    let term = staticDistance str
    in LEval.eval LEval.callByValueStep term

callByValueEvalTests :: IO TestTree
callByValueEvalTests = TestLambda.runShow "byValue" callByValueEval

infer :: String -> Infer.Result
infer = Infer.run . staticDistance

inferTests :: IO TestTree
inferTests = TestLambda.runShow "Infer" infer