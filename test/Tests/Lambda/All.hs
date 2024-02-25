module Tests.Lambda.All(tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Lambda.Common as TestLambda (run)

import qualified Lambda.Convert     as LC (e2t, t2e, Context)

import qualified Lambda.Expr        as LE (Expr)
import qualified Lambda.Term        as LT (Term)

import qualified Lambda.Lexer       as LL (alexScanTokens, Token)
import qualified Lambda.Parser      as LR (run)


tests :: IO TestTree
tests = testGroup "Lambda" <$>
    sequence [
        lexerTests,
        parserTests,
        staticDistanceTests,
        backToNamesTests
    ]

lexer :: String -> [LL.Token]
lexer = LL.alexScanTokens

lexerTests :: IO TestTree
lexerTests = TestLambda.run "Lexer" lexer

parser :: String -> LE.Expr
parser = LR.run . lexer

parserTests :: IO TestTree
parserTests = TestLambda.run "Parser" parser

staticDistance :: String -> (LC.Context, LT.Term)
staticDistance = LC.e2t . parser

staticDistanceTests :: IO TestTree
staticDistanceTests = TestLambda.run "StaticDistance" staticDistance

backToNames :: String -> LE.Expr
backToNames str =
    let (ctx, term) = staticDistance str
    in LC.t2e ctx term

backToNamesTests :: IO TestTree
backToNamesTests = TestLambda.run "BackToNames" backToNames


