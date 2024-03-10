module Tests.Lambda.All(tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Lambda.Common as TestLambda (runShow, runString)

import qualified Lambda.Convert.ToTerm    as ToTerm (run, Result)
import qualified Lambda.Convert.ToExpr    as ToExpr (run)

import qualified Lambda.Expr        as LE (Expr)
import qualified Lambda.Term        as LT (Term)

import qualified Lambda.Lexer       as LL (alexScanTokens, Token)
import qualified Lambda.Parser      as LR (run)
import qualified Lambda.Eval        as LEval (callByValueStep, steps, eval)
import qualified Lambda.Infer       as Infer (run, Result, Info)

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

staticDistance :: String -> ToTerm.Result
staticDistance = ToTerm.run . parser

staticDistanceTests :: IO TestTree
staticDistanceTests = TestLambda.runShow "StaticDistance" staticDistance

backToNames :: String -> Either Infer.Info LE.Expr
backToNames str =
    let term = staticDistance str
    in ToExpr.run <$> term

backToNamesTests :: IO TestTree
backToNamesTests = TestLambda.runShow "BackToNames" backToNames

callByValueEvalSteps :: String -> String
callByValueEvalSteps str =
    let term = staticDistance str
        termSteps = (LEval.steps LEval.callByValueStep) <$> term
    in case termSteps of
        Left info -> show info
        Right termSteps' -> unlines $ show <$> termSteps'

callByValueEvalStepsTests :: IO TestTree
callByValueEvalStepsTests  = TestLambda.runString "byValueSteps" callByValueEvalSteps

callByValueEval :: String -> ToTerm.Result
callByValueEval str =
    let term = (staticDistance str :: ToTerm.Result)

    in (LEval.eval LEval.callByValueStep) <$> term

callByValueEvalTests :: IO TestTree
callByValueEvalTests = TestLambda.runShow "byValue" callByValueEval

infer :: String -> Infer.Result
infer s =
    let term = (staticDistance s :: ToTerm.Result)
    in Infer.run =<< (term :: ToTerm.Result)

inferTests :: IO TestTree
inferTests = TestLambda.runShow "Infer" infer