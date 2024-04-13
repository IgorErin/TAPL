module Tests.Lambda.All(tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Lambda.Common as TestLambda (runShow, runString)

import qualified Lambda.Expr.Raw    as LE (Expr, ShowRaw(..))
import qualified Lambda.Expr.Typed  as ET (run, Result, ShowTyped(..))
import qualified Lambda.Lexer       as LL (alexScanTokens, Token)
import qualified Lambda.Parser      as LR (run)

tests :: IO TestTree
tests = testGroup "Lambda" <$>
    sequence [
        lexerTests,
        parserTests,
        inferTests
    ]

lexer :: String -> [LL.Token]
lexer = LL.alexScanTokens

lexerTests :: IO TestTree
lexerTests = TestLambda.runShow "Lexer" lexer

parser :: String -> LE.Expr
parser = LR.run . lexer

parserTests :: IO TestTree
parserTests = TestLambda.runShow "Parser" (LE.ShowRaw . parser)

infer :: String -> ET.Result
infer s =
    let term = parser s
    in ET.run term

inferTests :: IO TestTree
inferTests = TestLambda.runShow "Typed" ((ET.ShowTyped <$>) . infer)