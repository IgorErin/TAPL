module Tests.Lambda.All(tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Lambda.Common as TestLambda (runShow, runString)

import qualified Lambda.Convert     as LC (e2t, t2e, Context)

import qualified Lambda.Expr        as LE (Expr)
import qualified Lambda.Term        as LT (Term)

import qualified Lambda.Lexer       as LL (alexScanTokens, Token)
import qualified Lambda.Parser      as LR (run)
import qualified Lambda.Eval        as LEval (callByValueStep, steps, eval)
import qualified Lambda.Infer       as Infer (run)

import qualified Lambda.Types       as Types (Type)


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

staticDistance :: String -> (LC.Context, LT.Term)
staticDistance = LC.e2t . parser

staticDistanceTests :: IO TestTree
staticDistanceTests = TestLambda.runShow "StaticDistance" staticDistance

backToNames :: String -> LE.Expr
backToNames str =
    let (ctx, term) = staticDistance str
    in LC.t2e ctx term

backToNamesTests :: IO TestTree
backToNamesTests = TestLambda.runShow "BackToNames" backToNames

callByValueEvalSteps :: String -> String
callByValueEvalSteps str =
    let (ctx, term) = staticDistance str
        termSteps = LEval.steps LEval.callByValueStep term
        str' = unlines $ map show termSteps
    in case ctx of
        [] -> str'
        _ : _ -> error $ "Free vasr in term: " ++ show ctx

callByValueEvalStepsTests :: IO TestTree
callByValueEvalStepsTests  = TestLambda.runString "byValueSteps" callByValueEvalSteps

callByValueEval :: String -> LT.Term
callByValueEval str =
    let (ctx, term) = staticDistance str
    in case ctx of
        [] -> LEval.eval LEval.callByValueStep term
        _ : _ -> error $ "Free vasr in term: " ++ show ctx

callByValueEvalTests :: IO TestTree
callByValueEvalTests = TestLambda.runShow "byValue" callByValueEval

infer :: String -> Maybe Types.Type
infer = Infer.run . snd . staticDistance

inferTests :: IO TestTree
inferTests = TestLambda.runShow "Infer" infer